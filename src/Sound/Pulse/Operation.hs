{-
    Copyright 2016 Markus Ongyerth

    This file is part of pulseaudio-hs.

    Monky is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Monky is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with pulseaudio-hs.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-|
Module      : Sound.Pulse.Operation
Description : provides wrappers for pa_operation
Maintianer  : ongy
Stability   : experimental
-}
module Sound.Pulse.Operation
    ( Operation
    , UOperation

    , ptrToOperation
    , operationCancel
    , operationGetState
    , operationSetCallback
    )
where

import Sound.Pulse.Def (OperationState(..), operationStateFromInt)
import Sound.Pulse.Userdata
import Foreign.C.Types (CInt(..))
import Foreign.Ptr
import Foreign.ForeignPtr

-- |Internal Operation type, used for type safety
data IOperation
-- |Unsafe Operation. Should be return value of foreign import calls
data UOperation

-- |"High"-level operation type, this should be the return type for exported
-- |functions
data Operation = Operation (ForeignPtr IOperation)

type OperationNotifyCB = Ptr IOperation -> Ptr Userdata -> IO ()
foreign import ccall "wrapper" mkOperationNCB :: OperationNotifyCB -> IO (FunPtr OperationNotifyCB)

foreign import ccall "pa_operation_cancel" pa_operation_cancel :: Ptr IOperation -> IO ()
foreign import ccall "pa_operation_set_state_callback" pa_operation_set_state_callback :: Ptr IOperation -> FunPtr OperationNotifyCB -> Ptr Userdata -> IO ()
foreign import ccall "pa_operation_get_state" pa_operation_get_state :: Ptr IOperation -> IO CInt
foreign import ccall "&pa_operation_unref" pa_operation_unref :: FunPtr (Ptr IOperation -> IO ())

-- |Cancel an operation. The server may still do it, but the client will not be
-- |notified
operationCancel :: Operation -> IO ()
operationCancel (Operation i) = withForeignPtr i pa_operation_cancel

-- |This currently leaks the function, after the operation went out of scope
-- |I'm not quite sure when this will be called either way.
operationSetCallback :: Operation -> IO () -> IO ()
operationSetCallback (Operation i) fun = do
    funP <- mkOperationNCB $ \_ _ -> fun
    withForeignPtr i $ \x -> pa_operation_set_state_callback x funP nullPtr

-- |Get the State of the operation
operationGetState :: Operation -> IO OperationState
operationGetState (Operation i) = operationStateFromInt <$> withForeignPtr i pa_operation_get_state

-- |Convert a Unsafe Operation to a safe one. This should be called by wrappers
-- |to FFI imports.
-- |Also call this when not returning the value, since it registers the unref
-- |function for the operation.
ptrToOperation :: Ptr UOperation -> IO Operation
ptrToOperation = fmap Operation . newForeignPtr pa_operation_unref . castPtr

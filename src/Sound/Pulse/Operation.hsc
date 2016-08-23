{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Sound.Pulse.Operation
    ( Operation
    , UOperation

    , ptrToOperation
    , operationCancel
    , operationGetState
    , operationSetCallback
    )
where

#include <pulse/def.h>

import Sound.Pulse.Userdata
import Foreign.C.Types (CInt(..))
import Foreign.Ptr
import Foreign.ForeignPtr

data IOperation
data UOperation

data Operation = Operation (ForeignPtr IOperation)

data OperationState
    = OperationRunning
    | OperationDone
    | OperationCancelled
    deriving (Eq, Show)

operationStateFromInt :: CInt -> OperationState
operationStateFromInt i
    | i == #{const PA_OPERATION_RUNNING}   = OperationRunning
    | i == #{const PA_OPERATION_DONE}      = OperationDone
    | i == #{const PA_OPERATION_CANCELLED} = OperationCancelled
    | otherwise = error "PA: got unexpted operation state"

type OperationNotifyCB = Ptr IOperation -> Ptr Userdata -> IO ()
foreign import ccall "wrapper" mkOperationNCB :: OperationNotifyCB -> IO (FunPtr OperationNotifyCB)

foreign import ccall "pa_operation_cancel" pa_operation_cancel :: Ptr IOperation -> IO ()
foreign import ccall "pa_operation_set_state_callback" pa_operation_set_state_callback :: Ptr IOperation -> FunPtr OperationNotifyCB -> Ptr Userdata -> IO ()
foreign import ccall "pa_operation_get_state" pa_operation_get_state :: Ptr IOperation -> IO CInt
foreign import ccall "&pa_operation_unref" pa_operation_unref :: FunPtr (Ptr IOperation -> IO ())

operationCancel :: Operation -> IO ()
operationCancel (Operation i) = withForeignPtr i pa_operation_cancel

operationSetCallback :: Operation -> IO () -> IO ()
operationSetCallback (Operation i) fun = do
    funP <- mkOperationNCB $ \_ _ -> fun
    withForeignPtr i $ \x -> pa_operation_set_state_callback x funP nullPtr

operationGetState :: Operation -> IO OperationState
operationGetState (Operation i) = operationStateFromInt <$> withForeignPtr i pa_operation_get_state

ptrToOperation :: Ptr UOperation -> IO Operation
ptrToOperation = fmap Operation . newForeignPtr pa_operation_unref . castPtr

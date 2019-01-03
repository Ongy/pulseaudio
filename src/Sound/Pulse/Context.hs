{-# LANGUAGE OverloadedStrings #-}
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
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Sound.Pulse.Context
Description : provices the Context type and related functions
Maintianer  : ongy
Stability   : experimental

The 'Context' is the main object for the pulseaudio api. Most functions that
communicate with the server, are defined on the 'Context'.
-}
module Sound.Pulse.Context
    ( Context
    , ContextFlags(..)
    , ContextState(..)
    , ContextSuccessCB

    , getContext
    , connectContext
    , setStateCallback
    , getContextServer
    , getContextState
    , getContextErr
    , getContextErrStr
    , wrapSuccess
    )
where

import           Control.Applicative   ((<$>))
import           Control.Monad         ((<=<))
import           Data.Text             (Text)
import           Foreign.C             (CString)
import           Foreign.C.Types
import           Foreign.Marshal.Utils
import           Foreign.Ptr

import           Sound.Pulse.Def       (ContextState(..), contextStateFromInt, ContextFlags(..), contextFlagssToInt)
import           Sound.Pulse.Mainloop
import           Sound.Pulse.Userdata

import           Foreign

-- |Typesafety internal type
data CInternal
-- |Type we will be using for pa_context
type Context = Ptr CInternal

-- TODO: This here
data PASpawnApi

-- |Callback type for functions that only report success
type ContextSuccessCB = Context -> CInt -> Ptr Userdata -> IO ()
foreign import ccall "wrapper" mkCSuccess :: ContextSuccessCB -> IO (FunPtr ContextSuccessCB)

-- |Wrapp a function callback for 'ContextSuccessCB'
wrapSuccess
    :: (Bool -> IO ())
    -> IO (FunPtr ContextSuccessCB)
-- TODO: should this auto-cleanup itself?
wrapSuccess fun = mkCSuccess $ \_ b _ -> fun (b /= 0)

foreign import ccall "pa_context_new" pa_context_new :: Ptr a -> CString -> IO Context

foreign import ccall "pa_context_connect" pa_context_connect :: Context -> CString -> CInt -> Ptr PASpawnApi -> IO CInt

type ContextNotify a = Context -> Ptr a -> IO ()
foreign import ccall "wrapper" mkCNotify :: ContextNotify a -> IO (FunPtr (ContextNotify a))

foreign import ccall "pa_context_set_state_callback" pa_context_set_state_callback :: Context -> FunPtr (ContextNotify a) -> Ptr a -> IO ()
foreign import ccall "pa_context_get_server" pa_context_get_server :: Context -> IO CString

foreign import ccall "pa_context_get_state" pa_context_get_state :: Context -> IO CInt
foreign import ccall "pa_context_errno" pa_context_errno :: Context -> IO CInt

foreign import ccall "pa_strerror" pa_strerror :: CInt-> CString

-- |Create a pulseaudio context
getContext
    :: PAMainloop a
    => a -- ^The mainloop implementation
    -> Text -- ^The application name
    -> IO Context
getContext impl name = do
    ptr <- new =<< getMainloopApi impl
    withCStringText name (pa_context_new ptr)

-- |Connect a Context to a pulseaudio server.
connectContext
    :: Context -- ^The context
    -> Maybe Text -- ^The server to connect to. If this is Nothing, connect to the default server.
    -> [ContextFlags] -- ^Flags to control the startup behaviour of the server.
    -- -> SpawnApi! -- TODO
    -> IO (Maybe Int)
connectContext cxt serv flags = do
    let wrapper = maybe ($ nullPtr) (withCStringText) serv
    ret <- wrapper (\ptr -> pa_context_connect cxt ptr (contextFlagssToInt flags) nullPtr)
    if ret /= 0
       then Just <$> getContextErr cxt
       else return Nothing

-- |This callback is leaked! if it's reset
-- IMO the handler should stay forever aswell (even if just for loggin), so don't worry about it.
-- This should only be called once per application run though, so it will be a
-- known issue for know.
setStateCallback
    :: Context
    -> IO ()
    -> IO ()
setStateCallback cxt fun = do
    funP <- mkCNotify (\_ _ -> fun)
    pa_context_set_state_callback cxt funP nullPtr

-- |Get the Servername from a (connected) 'Context'.
getContextServer :: Context -> IO (Maybe Text)
getContextServer cxt = do
    cstr <- pa_context_get_server cxt
    if cstr == nullPtr
       then return Nothing
       else Just <$> peekCStringText cstr

-- |Get the current state from a 'Context'.
getContextState :: Context -> IO ContextState
getContextState = fmap contextStateFromInt . pa_context_get_state

-- |Get the last error from a 'Context'.
getContextErr :: Context -> IO Int
getContextErr = fmap fromIntegral . pa_context_errno

-- |Get the last error from a 'Context' in a human readable 'Text'.
getContextErrStr :: Context -> IO Text
getContextErrStr = peekCStringText . pa_strerror . fromIntegral <=< getContextErr

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr

import Sound.Pulse.Def (ContextState(..), contextStateFromInt, ContextFlags(..), contextFlagssToInt)
import Sound.Pulse.Mainloop
import Sound.Pulse.Userdata

-- |Typesafety internal type
data CInternal
-- |Type we will be using for PA Contexts
type Context = Ptr CInternal

-- TODO: This here
data PASpawnApi

type ContextSuccessCB = Context -> CInt -> Ptr Userdata -> IO ()
foreign import ccall "wrapper" mkCSuccess :: ContextSuccessCB -> IO (FunPtr ContextSuccessCB)

wrapSuccess :: (Bool -> IO ()) -> IO (FunPtr ContextSuccessCB)
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
    -> String -- ^The application name
    -> IO Context
getContext impl name = do
    ptr <- new =<< getMainloopApi impl
    withCString name (pa_context_new ptr)


connectContext
    :: Context
    -> Maybe String
    -> [ContextFlags]
    -- -> SpawnApi!
    -> IO ()
connectContext cxt serv flags = do
    let wrapper = maybe ($ nullPtr) (withCString) serv
    ret <- wrapper (\ptr -> pa_context_connect cxt ptr (contextFlagssToInt flags) nullPtr)
    if ret /= 0
       then error ("Failed to connect to server :( " ++ show ret)
       else return ()

setStateCallback
    :: Context
    -> IO ()
    -> IO ()
setStateCallback cxt fun = do
    funP <- mkCNotify (\_ _ -> fun)
    pa_context_set_state_callback cxt funP nullPtr

getContextServer :: Context -> IO (Maybe String)
getContextServer cxt = do
    cstr <- pa_context_get_server cxt
    if cstr == nullPtr
       then return Nothing
       else Just <$> peekCString cstr

getContextState :: Context -> IO ContextState
getContextState = fmap contextStateFromInt . pa_context_get_state

getContextErr :: Context -> IO Int
getContextErr = fmap fromIntegral . pa_context_errno

getContextErrStr :: Context -> IO String
getContextErrStr cxt = do
    num <- getContextErr cxt
    peekCString $ pa_strerror $ fromIntegral num

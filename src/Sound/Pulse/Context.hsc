{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.Pulse.Context
    ( PAContext
    , PAContextFlags(..)
    , PAContextState(..)
    , PAContextSuccessCB

    , getPAContext
    , connectPAContext
    , setPAStateCallback
    , getPAContextServer
    , getPAContextState
    , getPAContextErr
    , getPAContextErrStr
    , wrapSuccess
    )
where


#include <pulse/context.h>

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Sound.Pulse.Mainloop

-- |Typesafety internal type
data PACInternal
-- |Type we will be using for PA Contexts
type PAContext = Ptr PACInternal

-- TODO: implement when I feel like it :)
data PAContextFlags
    = PAContextNoFlags
    | PAContextNoAutospawn
    | PAContextNoFail

-- TODO: This here
data PASpawnApi

data PAContextState
    = PAContextUnconnectd
    | PAContextConnecting
    | PAContextAuthorizing
    | PAContextSettingName
    | PAContextReady
    | PAContextFailed
    | PAContextTerminated
    deriving (Eq, Show)

paContextStateToInt :: PAContextState -> CInt
paContextStateToInt PAContextUnconnectd  = #{const PA_CONTEXT_UNCONNECTED}
paContextStateToInt PAContextConnecting  = #{const PA_CONTEXT_CONNECTING}
paContextStateToInt PAContextAuthorizing = #{const PA_CONTEXT_AUTHORIZING}
paContextStateToInt PAContextSettingName = #{const PA_CONTEXT_SETTING_NAME}
paContextStateToInt PAContextReady       = #{const PA_CONTEXT_READY}
paContextStateToInt PAContextFailed      = #{const PA_CONTEXT_FAILED}
paContextStateToInt PAContextTerminated  = #{const PA_CONTEXT_TERMINATED}

paIntToContextState :: CInt -> PAContextState
paIntToContextState i
  | i == #{const PA_CONTEXT_UNCONNECTED}  = PAContextUnconnectd
  | i == #{const PA_CONTEXT_CONNECTING}   = PAContextConnecting
  | i == #{const PA_CONTEXT_AUTHORIZING}  = PAContextAuthorizing
  | i == #{const PA_CONTEXT_SETTING_NAME} = PAContextSettingName
  | i == #{const PA_CONTEXT_READY}        = PAContextReady
  | i == #{const PA_CONTEXT_FAILED}       = PAContextFailed
  | i == #{const PA_CONTEXT_TERMINATED}   = PAContextTerminated
  | otherwise                                   = error "Got an unkown PAContextState"

instance Storable PAContextState where
    sizeOf _ = sizeOf (undefined :: CInt)
    alignment _ = alignment (undefined :: CInt)
    poke p s = poke (castPtr p) (paContextStateToInt s)
    peek p = paIntToContextState <$> peek (castPtr p)

type PAContextSuccessCB a = PAContext -> CInt -> Ptr a -> IO ()
foreign import ccall "wrapper" mkCSuccess :: PAContextSuccessCB a -> IO (FunPtr (PAContextSuccessCB a))

wrapSuccess :: (Bool -> IO ()) -> IO (FunPtr (PAContextSuccessCB a))
wrapSuccess fun = mkCSuccess $ \_ b _ -> fun (b /= 0)

foreign import ccall "pa_context_new" pa_context_new :: Ptr a -> CString -> IO PAContext

-- foreign import ccall "pa_context_connect" pa_conetxt_connect :: PAContext -> CString -> [PAContextFlags] -> Ptr PASpawnApi -> IO ()
foreign import ccall "pa_context_connect" pa_context_connect :: PAContext -> CString -> CInt -> Ptr PASpawnApi -> IO CInt


type PAContextNotify a = PAContext -> Ptr a -> IO ()
foreign import ccall "wrapper" mkCNotify :: PAContextNotify a -> IO (FunPtr (PAContextNotify a))

foreign import ccall "pa_context_set_state_callback" pa_context_set_state_callback :: PAContext -> FunPtr (PAContextNotify a) -> Ptr a -> IO ()
foreign import ccall "pa_context_get_server" pa_context_get_server :: PAContext -> IO CString

foreign import ccall "pa_context_get_state" pa_context_get_state :: PAContext -> IO CInt
foreign import ccall "pa_context_errno" pa_context_errno :: PAContext -> IO CInt

foreign import ccall "pa_strerror" pa_strerror :: CInt-> CString

-- |Create a pulseaudio context
getPAContext
    :: PAMainloop a
    => a -- ^The mainloop implementation
    -> String -- ^The application name
    -> IO PAContext
getPAContext impl name = do
    ptr <- new =<< getMainloopApi impl
    withCString name (pa_context_new ptr)


connectPAContext
    :: PAContext
    -> Maybe String
    -> [PAContextFlags]
    -- -> SpawnApi!
    -> IO ()
connectPAContext cxt serv _ = do
    let wrapper = maybe ($ nullPtr) (withCString) serv
    ret <- wrapper (\ptr -> pa_context_connect cxt ptr 0 nullPtr)
    if ret /= 0
       then error ("Failed to connect to server :( " ++ show ret)
       else return ()

setPAStateCallback
    :: PAContext
    -> (PAContext -> IO ())
    -> IO ()
setPAStateCallback cxt fun = do
    funP <- mkCNotify (\c _ -> fun c)
    pa_context_set_state_callback cxt funP nullPtr

getPAContextServer :: PAContext -> IO (Maybe String)
getPAContextServer cxt = do
    cstr <- pa_context_get_server cxt
    if cstr == nullPtr
       then return Nothing
       else Just <$> peekCString cstr

getPAContextState :: PAContext -> IO PAContextState
getPAContextState = fmap paIntToContextState . pa_context_get_state

getPAContextErr :: PAContext -> IO Int
getPAContextErr = fmap fromIntegral . pa_context_errno

getPAContextErrStr :: PAContext -> IO String
getPAContextErrStr cxt = do
    num <- getPAContextErr cxt
    peekCString $ pa_strerror $ fromIntegral num

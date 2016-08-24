{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.Pulse.Serverinfo
where

#include <pulse/introspect.h>

import Data.Word (Word32)
import Sound.Pulse.SampleSpec
import Sound.Pulse.ChannelPosition
import Sound.Pulse.Context
import Sound.Pulse.Userdata
import Sound.Pulse.Operation

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.String

data ServerInfo = ServerInfo
    { userName          :: String
    , hostName          :: String
    , serverVersion     :: String
    , serverName        :: String
    , sampleSpec        :: SampleSpec
    , defaultSinkName   :: String
    , defaultSourceName :: String
    , cookie            :: Word32
    , channelMap        :: ChannelMap
    } deriving (Eq, Show)

instance Storable ServerInfo where
    sizeOf _ = #{size struct pa_server_info}
    alignment _ = #{alignment struct pa_server_info}
    peek p = ServerInfo
        <$> (peekCString =<< #{peek struct pa_server_info, user_name} p)
        <*> (peekCString =<< #{peek struct pa_server_info, host_name} p)
        <*> (peekCString =<< #{peek struct pa_server_info, server_version} p)
        <*> (peekCString =<< #{peek struct pa_server_info, server_name} p)
        <*> #{peek struct pa_server_info, sample_spec} p
        <*> (peekCString =<< #{peek struct pa_server_info, default_sink_name} p)
        <*> (peekCString =<< #{peek struct pa_server_info, default_source_name} p)
        <*> #{peek struct pa_server_info, cookie} p
        <*> #{peek struct pa_server_info, channel_map} p
    
    poke _ _ = error "PA: no poke for ServerInfo yet" -- TODO

type ServerInfoCB = Context -> Ptr ServerInfo -> Ptr Userdata -> IO ()
foreign import ccall "wrapper" mkServerInfoCB :: ServerInfoCB -> IO (FunPtr ServerInfoCB)

foreign import ccall "pa_context_get_server_info" pa_context_get_server_info :: Context -> FunPtr ServerInfoCB -> Ptr Userdata -> IO (Ptr UOperation)

getServerInfo
    :: Context
    -> (ServerInfo -> IO ())
    -> IO ()
getServerInfo cxt fun = do
    funP <- mkServerInfoCB $ \_ ptr fP -> do
        fun =<< peek ptr
        freeHaskellFunPtr (castPtrToFunPtr fP)
    _ <- ptrToOperation =<< pa_context_get_server_info cxt funP (castFunPtrToPtr funP)
    return ()

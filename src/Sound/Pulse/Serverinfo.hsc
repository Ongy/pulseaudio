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
{-|
Module      : Sound.Pulse.Serverinfo
Description : provides the time type used for pa_server_info.
Maintianer  : ongy
Stability   : experimental
-}
module Sound.Pulse.Serverinfo
    ( ServerInfo(..)
    , getServerInfo
    , getServerInfoM
    )
where

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif
#include <pulse/introspect.h>

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import Data.Word (Word32, Word)
import Sound.Pulse
import Sound.Pulse.ChannelPosition
import Sound.Pulse.Context
import Sound.Pulse.Operation
import Sound.Pulse.SampleSpec
import Sound.Pulse.Userdata

import Foreign.Storable
import Foreign.Ptr

import Foreign

-- |The type used for pa_server_info
data ServerInfo = ServerInfo
    { userName          :: Text
    , hostName          :: Text
    , serverVersion     :: Text
    , serverName        :: Text
    , sampleSpec        :: SampleSpec
    , defaultSinkName   :: Text
    , defaultSourceName :: Text
    , cookie            :: Word32
    , channelMap        :: ChannelMap
    } deriving (Eq, Show)

instance Storable ServerInfo where
    sizeOf _ = #{size struct pa_server_info}
    alignment _ = #{alignment struct pa_server_info}
    peek p = ServerInfo
        <$> (peekCStringText =<< #{peek struct pa_server_info, user_name} p)
        <*> (peekCStringText =<< #{peek struct pa_server_info, host_name} p)
        <*> (peekCStringText =<< #{peek struct pa_server_info, server_version} p)
        <*> (peekCStringText =<< #{peek struct pa_server_info, server_name} p)
        <*> #{peek struct pa_server_info, sample_spec} p
        <*> (peekCStringText =<< #{peek struct pa_server_info, default_sink_name} p)
        <*> (peekCStringText =<< #{peek struct pa_server_info, default_source_name} p)
        <*> #{peek struct pa_server_info, cookie} p
        <*> #{peek struct pa_server_info, channel_map} p

    poke _ _ = error "PA: no poke for ServerInfo yet" -- TODO

type ServerInfoCB = Context -> Ptr ServerInfo -> Ptr Userdata -> IO ()
foreign import ccall "wrapper" mkServerInfoCB :: ServerInfoCB -> IO (FunPtr ServerInfoCB)

foreign import ccall "pa_context_get_server_info" pa_context_get_server_info :: Context -> FunPtr ServerInfoCB -> Ptr Userdata -> IO (Ptr UOperation)

-- |Get the server info.
getServerInfo
    :: Context -- ^Context for the server connection
    -> (ServerInfo -> IO ())
    -> IO ()
getServerInfo cxt fun = do
    funP <- mkServerInfoCB $ \_ ptr fP -> do
        fun =<< peek ptr
        freeHaskellFunPtr (castPtrToFunPtr fP)
    _ <- ptrToOperation =<< pa_context_get_server_info cxt funP (castFunPtrToPtr funP)
    return ()

getServerInfoM :: Pulse ServerInfo
getServerInfoM = Pulse getServerInfo

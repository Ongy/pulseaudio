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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Sound.Pulse.Sinkinfo
Description : provides the time type used for pa_sink_info.
Maintianer  : ongy
Stability   : experimental
-}
module Sound.Pulse.Sinkinfo
    ( SinkFlags(..)
    , SinkState(..)
    , Sinkinfo(..)

    , getContextSinks
    , getContextSinkByName
    , getContextSinkByIndex
    )
where

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif
#include <pulse/introspect.h>

import Control.Applicative ((<$>), (<*>))
import Sound.Pulse.Volume
import Sound.Pulse.Operation
import Sound.Pulse.Userdata
import Data.Word (Word32, Word8)

import Foreign.Ptr (Ptr, FunPtr, freeHaskellFunPtr, castFunPtrToPtr, castPtrToFunPtr)
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.C.String (peekCString, withCString, CString)
import Foreign.Storable (Storable(..))

import Sound.Pulse.Context (Context)
import Sound.Pulse.ChannelPosition
import Sound.Pulse.SampleSpec

import Sound.Pulse.Def (SinkFlags(..), sinkFlagssFromInt, SinkState(..), sinkStateFromInt)

data PropList -- TODO :)

data FormatInfo -- TODO
data SinkPortInfo -- TODO

-- |Type used for pa_sink_info
data Sinkinfo = Sinkinfo
    { siName              :: String
    , siIndex             :: Word32
    , siDescription       :: String
    , siSampleSpec        :: SampleSpec
    , siChannelMap        :: ChannelMap
    , siOwnerModule       :: Word32
    , siVolume            :: CVolume
    , siMute              :: Bool
    , siMonitorSource     :: Word32
    , siMonitorSourceName :: String
    , siLatency           :: Word
    , siDriver            :: String
    , siFlags             :: [SinkFlags]
    , siProplist          :: Ptr PropList
    , siConfiguredLatency :: Word
    , siBaseVolume        :: Volume
    , siState             :: SinkState
    , siVolumeSteps       :: Word32
    , siCard              :: Word32
    , siPorts             :: [Ptr SinkPortInfo]
    , siActivePort        :: Ptr SinkPortInfo
    , siFormats           :: [Ptr FormatInfo]
    } deriving (Eq, Show)

instance Storable Sinkinfo where
    sizeOf _ = #{size struct pa_sink_info}
    alignment _ = #{alignment struct pa_sink_info}
    peek p = Sinkinfo
       <$> (peekCString =<< #{peek struct pa_sink_info, name} p)
       <*> #{peek struct pa_sink_info, index} p
       <*> (peekCString =<< #{peek struct pa_sink_info, description} p)
       <*> #{peek struct pa_sink_info, sample_spec} p
       <*> #{peek struct pa_sink_info, channel_map} p
       <*> #{peek struct pa_sink_info, owner_module} p
       <*> #{peek struct pa_sink_info, volume} p
       <*> ((/= (0 :: CInt)) <$> (#{peek struct pa_sink_info, mute} p))
       <*> #{peek struct pa_sink_info, monitor_source} p
       <*> (peekCString =<< #{peek struct pa_sink_info, monitor_source_name} p)
       <*> #{peek struct pa_sink_info, latency} p
       <*> (peekCString =<< #{peek struct pa_sink_info, driver} p)
       <*> (sinkFlagssFromInt <$> (#{peek struct pa_sink_info, mute} p))
       <*> #{peek struct pa_sink_info, proplist} p
       <*> #{peek struct pa_sink_info, configured_latency} p
       <*> #{peek struct pa_sink_info, base_volume} p
       <*> (sinkStateFromInt <$> (#{peek struct pa_sink_info, mute} p))
       <*> #{peek struct pa_sink_info, n_volume_steps} p
       <*> #{peek struct pa_sink_info, card} p
       <*> do
           size :: Word8 <- #{peek struct pa_sink_info, n_ports} p
           ptr <- #{peek struct pa_sink_info, ports} p
           mapM (peekElemOff ptr . fromIntegral) [0.. size - 1]
       <*> #{peek struct pa_sink_info, active_port} p
       <*> do
           size :: Word8 <- #{peek struct pa_sink_info, n_formats} p
           ptr :: Ptr (Ptr FormatInfo) <- #{peek struct pa_sink_info, formats} p
           mapM (peekElemOff ptr . fromIntegral) [0.. size - 1]
    poke _ (Sinkinfo {..}) = error "PA: Currently no sinkinfo poke"

type SinkinfoCB = Context -> Ptr Sinkinfo -> CInt -> Ptr Userdata -> IO ()
foreign import ccall "wrapper" mkSinkinfoCB :: SinkinfoCB -> IO (FunPtr SinkinfoCB)

foreign import ccall "pa_context_get_sink_info_list" pa_context_get_sink_info_list :: Context -> FunPtr SinkinfoCB -> Ptr Userdata -> IO (Ptr UOperation)

foreign import ccall "pa_context_get_sink_info_by_name" pa_context_get_sink_info_by_name :: Context -> CString -> FunPtr SinkinfoCB -> Ptr Userdata -> IO (Ptr UOperation)

foreign import ccall "pa_context_get_sink_info_by_index" pa_context_get_sink_info_by_index :: Context -> CUInt -> FunPtr SinkinfoCB -> Ptr Userdata -> IO (Ptr UOperation)

mkCallback :: (Sinkinfo -> IO ()) -> IO () -> IO (FunPtr SinkinfoCB)
mkCallback fun endf = mkSinkinfoCB $
    \_ ptr end fP -> if end == 0
        then fun =<< peek ptr
        else do
            endf -- Call the user end function
            -- free the FunPtr defiend here
            freeHaskellFunPtr (castPtrToFunPtr fP)

-- |Get all sinks from a context.
getContextSinks
    :: Context -- ^The context
    -> (Sinkinfo -> IO ()) -- ^List callback. Will be called once per list entry
    -> IO () -- ^End callback. Will be called once after all list entries
    -> IO Operation
getContextSinks cxt fun endf = do
    funP <- mkCallback fun endf
    ptrToOperation =<< pa_context_get_sink_info_list cxt funP (castFunPtrToPtr funP)

-- |Get a sink by name
getContextSinkByName
    :: Context
    -> String
    -> (Sinkinfo -> IO ())
    -> IO Operation
getContextSinkByName cxt name fun = do
    funP <- mkCallback fun (return ())
    ptrToOperation =<< withCString name (\ptr -> pa_context_get_sink_info_by_name cxt ptr funP (castFunPtrToPtr funP))

-- |Get a sink by index
getContextSinkByIndex
    :: Context
    -> Word32
    -> (Sinkinfo -> IO ())
    -> IO Operation
getContextSinkByIndex cxt idx fun = do
    funP <- mkCallback fun (return ())
    ptrToOperation =<< pa_context_get_sink_info_by_index cxt (fromIntegral idx) funP (castFunPtrToPtr funP)

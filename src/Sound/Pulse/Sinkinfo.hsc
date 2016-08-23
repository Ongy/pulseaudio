{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sound.Pulse.Sinkinfo
    ( CVolume(..)
    , Volume
    , SinkFlags(..)
    , SinkState(..)

    , getContextSinks
    )
where

#include <pulse/introspect.h>

import Sound.Pulse.Operation
import Sound.Pulse.Userdata
import Data.List (genericLength)
import Data.Word (Word32, Word8)

import Foreign.Ptr (Ptr, plusPtr, FunPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.C.Types (CInt(..))
import Foreign.C.String (peekCString)

import Sound.Pulse.Context (Context)
import Sound.Pulse.ChannelPosition
import Sound.Pulse.SampleSpec

import Sound.Pulse.Def (SinkFlags(..), sinkFlagssFromInt, SinkState(..), sinkStateFromInt)

type Volume = Word32
newtype CVolume = CVolume [Volume] deriving (Eq, Show)

instance Storable CVolume where
    sizeOf _ = #{size struct pa_cvolume}
    alignment _ = alignment (undefined :: Word)
    peek p = do
        size :: Word8 <- #{peek struct pa_cvolume, channels} p
        ints <- mapM (peekElemOff (#{ptr struct pa_cvolume, values} p) . fromIntegral) [0 .. size - 1]
        return $ CVolume ints
    poke p (CVolume vols) = do
        #{poke struct pa_cvolume, channels} p $ (genericLength vols :: Word8)
        let indexed = zip [0..] vols
        mapM_ (uncurry (pokeElemOff (#{ptr struct pa_cvolume, values} p))) indexed

data PropList -- TODO :)

data FormatInfo
data SinkPortInfo

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
    , siBaseVolue         :: Volume
    , siState             :: SinkState
    , siVolumeSteps       :: Word32
    , siCard              :: Word32
    , siPorts             :: [Ptr SinkPortInfo]
    , siActivePort        :: Ptr SinkPortInfo
    , siFormats           :: [Ptr FormatInfo]
    } deriving (Eq, Show)

instance Storable Sinkinfo where
    sizeOf _ = #{size struct pa_sink_info}
    alignment _ = alignment (undefined :: Word)
    peek p = Sinkinfo
       <$> peekCString (#{ptr struct pa_sink_info, name} p)
       <*> #{peek struct pa_sink_info, index} p
       <*> peekCString (#{ptr struct pa_sink_info, description} p)
       <*> #{peek struct pa_sink_info, sample_spec} p
       <*> #{peek struct pa_sink_info, channel_map} p
       <*> #{peek struct pa_sink_info, owner_module} p
       <*> #{peek struct pa_sink_info, volume} p
       <*> ((/= (0 :: CInt)) <$> (#{peek struct pa_sink_info, mute} p))
       <*> #{peek struct pa_sink_info, monitor_source} p
       <*> peekCString (#{ptr struct pa_sink_info, monitor_source_name} p)
       <*> #{peek struct pa_sink_info, latency} p
       <*> peekCString (#{ptr struct pa_sink_info, driver} p)
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

getContextSinks :: Context -> (Sinkinfo -> IO ()) -> IO () -> IO Operation
getContextSinks cxt fun endf = do
    funP <- mkSinkinfoCB $ \_ ptr end _ -> if end == 0
                                              then fun =<< peek ptr
                                              else endf
    ptrToOperation =<< pa_context_get_sink_info_list cxt funP nullPtr

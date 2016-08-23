{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sound.Pulse.Sinkinfo
where

import Data.List (genericLength)
import Data.Word (Word32, Word8)

import Foreign.Ptr (Ptr, plusPtr, FunPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.C.Types (CInt(..))
import Foreign.C.String (peekCString)

import Sound.Pulse.Context (PAContext)
import Sound.Pulse.ChannelPosition
import Sound.Pulse.SampleSpec

import Data.Bits ((.|.), (.&.))

#include <pulse/introspect.h>

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

data SinkFlag
    = SinkHWVolumeCtrl
    | SinkLatency
    | SinkHardware
    | SinkNetwork
    | SinkHWMuteCtrl
    | SinkDecibelVolume
    | SinkFlatVolume
    | SinkDynamicLatency
    | SinkSetFormats
    deriving (Eq, Show)

sinkFlagToInt :: SinkFlag -> CInt
sinkFlagToInt SinkHWVolumeCtrl   = #{const PA_SINK_HW_VOLUME_CTRL}
sinkFlagToInt SinkLatency        = #{const PA_SINK_LATENCY}
sinkFlagToInt SinkHardware       = #{const PA_SINK_HARDWARE}
sinkFlagToInt SinkNetwork        = #{const PA_SINK_NETWORK}
sinkFlagToInt SinkHWMuteCtrl     = #{const PA_SINK_HW_MUTE_CTRL}
sinkFlagToInt SinkDecibelVolume  = #{const PA_SINK_DECIBEL_VOLUME}
sinkFlagToInt SinkFlatVolume     = #{const PA_SINK_FLAT_VOLUME}
sinkFlagToInt SinkDynamicLatency = #{const PA_SINK_DYNAMIC_LATENCY}
sinkFlagToInt SinkSetFormats     = #{const PA_SINK_SET_FORMATS}

sinkFlagsToInt :: [SinkFlag] -> CInt
sinkFlagsToInt = foldr (\x y -> sinkFlagToInt x .|. y) 0

sinkFlagsFromInt :: CInt -> [SinkFlag]
sinkFlagsFromInt v =
    let a = if (v .&. #{const PA_SINK_HW_VOLUME_CTRL}  /= 0) then (SinkHWVolumeCtrl  :) else id
        b = if (v .&. #{const PA_SINK_LATENCY}         /= 0) then (SinkLatency       :) else id
        c = if (v .&. #{const PA_SINK_HARDWARE}        /= 0) then (SinkHardware      :) else id
        d = if (v .&. #{const PA_SINK_NETWORK}         /= 0) then (SinkNetwork       :) else id
        e = if (v .&. #{const PA_SINK_HW_MUTE_CTRL}    /= 0) then (SinkHWMuteCtrl    :) else id
        f = if (v .&. #{const PA_SINK_DECIBEL_VOLUME}  /= 0) then (SinkDecibelVolume :) else id
        g = if (v .&. #{const PA_SINK_FLAT_VOLUME}     /= 0) then (SinkFlatVolume    :) else id
        h = if (v .&. #{const PA_SINK_DYNAMIC_LATENCY} /= 0) then (SinkDynamicLatency:) else id
        i = if (v .&. #{const PA_SINK_SET_FORMATS}     /= 0) then (SinkSetFormats    :) else id
    in a . b . c . d . e . f . g . h . i $ []

data PropList -- TODO :)

data SinkState
    = SinkInvalidState
    | SinkRunning
    | SinkIdle
    | SinkSuspended
    deriving (Eq, Show)

sinkStateToInt :: SinkState -> CInt
sinkStateToInt SinkInvalidState = #{const PA_SINK_INVALID_STATE}
sinkStateToInt SinkRunning      = #{const PA_SINK_RUNNING}
sinkStateToInt SinkIdle         = #{const PA_SINK_IDLE}
sinkStateToInt SinkSuspended    = #{const PA_SINK_SUSPENDED}

sinkStateFromInt :: CInt -> SinkState
sinkStateFromInt i
    | i == #{const PA_SINK_INVALID_STATE} = SinkInvalidState
    | i == #{const PA_SINK_RUNNING}       = SinkRunning
    | i == #{const PA_SINK_IDLE}          = SinkIdle
    | i == #{const PA_SINK_SUSPENDED}     = SinkSuspended
    | otherwise = error "PA: Encountered unexpected sinkstate"


data FormatInfo = FormatInfo
data SinkPortInfo = SinkPortInfo

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
    , siFlags             :: [SinkFlag]
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
       <*> (sinkFlagsFromInt <$> (#{peek struct pa_sink_info, mute} p))
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

data Userdata
data PAOperation -- < TODO!!
type SinkinfoCB = PAContext -> Ptr Sinkinfo -> CInt -> Ptr Userdata -> IO ()
foreign import ccall "wrapper" mkSinkinfoCB :: SinkinfoCB -> IO (FunPtr SinkinfoCB)

foreign import ccall "pa_context_get_sink_info_list" pa_context_get_sink_info_list :: PAContext -> FunPtr SinkinfoCB -> Ptr Userdata -> IO (Ptr PAOperation)

getContextSinks :: PAContext -> (Sinkinfo -> IO ()) -> IO ()-> IO ()
getContextSinks cxt fun endf = do
    funP <- mkSinkinfoCB $ \_ ptr end _ -> if (end == 0)
                                              then (fun =<< peek ptr)
                                              else endf
    _ <- pa_context_get_sink_info_list cxt funP nullPtr
    return ()


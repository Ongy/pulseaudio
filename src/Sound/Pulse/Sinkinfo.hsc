{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Sound.Pulse.Sinkinfo
    ( SinkFlags(..)
    , SinkState(..)
    , Sinkinfo(..)

    , getContextSinks
    , getContextSinkByName
    , getContextSinkByIndex
    )
where

#include <pulse/introspect.h>

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


getContextSinks :: Context -> (Sinkinfo -> IO ()) -> IO () -> IO Operation
getContextSinks cxt fun endf = do
    funP <- mkCallback fun endf
    ptrToOperation =<< pa_context_get_sink_info_list cxt funP (castFunPtrToPtr funP)

getContextSinkByName
    :: Context
    -> String
    -> (Sinkinfo -> IO ())
    -> IO Operation
getContextSinkByName cxt name fun = do
    funP <- mkCallback fun (return ())
    ptrToOperation =<< withCString name (\ptr -> pa_context_get_sink_info_by_name cxt ptr funP (castFunPtrToPtr funP))

getContextSinkByIndex
    :: Context
    -> Word32
    -> (Sinkinfo -> IO ())
    -> IO Operation
getContextSinkByIndex cxt idx fun = do
    funP <- mkCallback fun (return ())
    ptrToOperation =<< pa_context_get_sink_info_by_index cxt (fromIntegral idx) funP (castFunPtrToPtr funP)

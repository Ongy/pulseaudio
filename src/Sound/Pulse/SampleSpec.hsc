{-# LANGUAGE RecordWildCards #-}
module Sound.Pulse.SampleSpec
    ( SampleFormat(..)
    , SampleSpec(..)
    )
where

#include <pulse/sample.h>

import Sound.Pulse.Def (SampleFormat(..), sampleFormatFromInt, sampleFormatToInt)
import Foreign.Storable (Storable(..))

import Data.Word (Word32, Word8)

data SampleSpec = SampleSpec
    { ssFormat   :: SampleFormat
    , ssRate     :: Word32
    , ssChannels :: Word8
    } deriving (Eq, Show)

instance Storable SampleSpec where
    sizeOf _ = #{size struct pa_sample_spec}
    alignment _ = #{alignment struct pa_sample_spec}
    peek p = SampleSpec
        <$> (sampleFormatFromInt <$> #{peek struct pa_sample_spec, format} p)
        <*> #{peek struct pa_sample_spec, rate} p
        <*> #{peek struct pa_sample_spec, channels} p
    poke p (SampleSpec {..}) = do
        #{poke struct pa_sample_spec, format} p $ sampleFormatToInt ssFormat
        #{poke struct pa_sample_spec, rate} p ssRate
        #{poke struct pa_sample_spec, channels} p ssChannels

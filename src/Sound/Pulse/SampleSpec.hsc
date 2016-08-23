{-# LANGUAGE RecordWildCards #-}
module Sound.Pulse.SampleSpec
    ( SampleFormat(..)
    , SampleSpec(..)
    )
where

#include <pulse/sample.h>

import Foreign.Storable (Storable(..))
import Foreign.C.Types (CInt)

import Data.Word (Word32, Word8)

data SampleFormat
    = SampleU8
    | SampleAlaw
    | SampleULaw
    | SampleS16LE
    | SampleS16BE
    | SampleFloat32LE
    | SampleFloat32BE
    | SampleS32LE
    | SampleS32BE
    | SampleS24LE
    | SampleS24BE
    | SampleS24_32LE
    | SampleS24_32BE
    | SampleInvalid
    deriving (Eq, Show)

sampleToInt :: SampleFormat -> CInt
sampleToInt SampleU8        = #{const PA_SAMPLE_U8}
sampleToInt SampleAlaw      = #{const PA_SAMPLE_ALAW}
sampleToInt SampleULaw      = #{const PA_SAMPLE_ULAW}
sampleToInt SampleS16LE     = #{const PA_SAMPLE_S16LE}
sampleToInt SampleS16BE     = #{const PA_SAMPLE_S16BE}
sampleToInt SampleFloat32LE = #{const PA_SAMPLE_FLOAT32LE}
sampleToInt SampleFloat32BE = #{const PA_SAMPLE_FLOAT32BE}
sampleToInt SampleS32LE     = #{const PA_SAMPLE_S32LE}
sampleToInt SampleS32BE     = #{const PA_SAMPLE_S32BE}
sampleToInt SampleS24LE     = #{const PA_SAMPLE_S24LE}
sampleToInt SampleS24BE     = #{const PA_SAMPLE_S24BE}
sampleToInt SampleS24_32LE  = #{const PA_SAMPLE_S24_32LE}
sampleToInt SampleS24_32BE  = #{const PA_SAMPLE_S24_32BE}
sampleToInt SampleInvalid   = #{const PA_SAMPLE_INVALID}

sampleFromInt :: CInt -> SampleFormat
sampleFromInt i
  | i == #{const PA_SAMPLE_U8}        = SampleU8
  | i == #{const PA_SAMPLE_ALAW}      = SampleAlaw
  | i == #{const PA_SAMPLE_ULAW}      = SampleULaw
  | i == #{const PA_SAMPLE_S16LE}     = SampleS16LE
  | i == #{const PA_SAMPLE_S16BE}     = SampleS16BE
  | i == #{const PA_SAMPLE_FLOAT32LE} = SampleFloat32LE
  | i == #{const PA_SAMPLE_FLOAT32BE} = SampleFloat32BE
  | i == #{const PA_SAMPLE_S32LE}     = SampleS32LE
  | i == #{const PA_SAMPLE_S32BE}     = SampleS32BE
  | i == #{const PA_SAMPLE_S24LE}     = SampleS24LE
  | i == #{const PA_SAMPLE_S24BE}     = SampleS24BE
  | i == #{const PA_SAMPLE_S24_32LE}  = SampleS24_32LE
  | i == #{const PA_SAMPLE_S24_32BE}  = SampleS24_32BE
  | i == #{const PA_SAMPLE_INVALID}   = SampleInvalid
  | otherwise = error "PA: Unknown SampleFormat :("


data SampleSpec = SampleSpec
    { ssFormat   :: SampleFormat
    , ssRate     :: Word32
    , ssChannels :: Word8
    } deriving (Eq, Show)

instance Storable SampleSpec where
    sizeOf _ = #{size struct pa_sample_spec}
    alignment _ = alignment (undefined :: Word)
    peek p = SampleSpec
        <$> (sampleFromInt <$> #{peek struct pa_sample_spec, format} p)
        <*> #{peek struct pa_sample_spec, rate} p
        <*> #{peek struct pa_sample_spec, channels} p
    poke p (SampleSpec {..}) = do
        #{poke struct pa_sample_spec, format} p $ sampleToInt ssFormat
        #{poke struct pa_sample_spec, rate} p ssRate
        #{poke struct pa_sample_spec, channels} p ssChannels


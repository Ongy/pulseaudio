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
{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Sound.Pulse.SampleSpec
Description : provides the time type used for pa_sample_spec.
Maintianer  : ongy
Stability   : experimental
-}
module Sound.Pulse.SampleSpec
    ( SampleFormat(..)
    , SampleSpec(..)
    )
where

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif
#include <pulse/sample.h>

import Control.Applicative ((<$>))
import Sound.Pulse.Def (SampleFormat(..), sampleFormatFromInt, sampleFormatToInt)
import Foreign.Storable (Storable(..))

import Data.Word (Word32, Word8)

-- |The pa_sample_spec type for Haskell.
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

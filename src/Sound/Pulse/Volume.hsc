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
{-|
Module      : Sound.Pulse.volume
Description : provides the types for volume
Maintianer  : ongy
Stability   : experimental
-}
module Sound.Pulse.Volume
    ( Volume
    , CVolume(..)

    , volumeToLinear
    , cVolumeToLinear
    )
where

#include <pulse/introspect.h>

import Data.Word (Word32, Word8)
import Data.List (genericLength)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (plusPtr)

-- |type for pa_volume_t
type Volume = Word32

-- |type for pa_cvolume. This contains a 'Volume' for each channel.
newtype CVolume = CVolume [Volume] deriving (Eq, Show)

instance Storable CVolume where
    sizeOf _ = #{size struct pa_cvolume}
    alignment _ = #{alignment struct pa_cvolume}
    peek p = do
        size :: Word8 <- #{peek struct pa_cvolume, channels} p
        ints <- mapM (peekElemOff (#{ptr struct pa_cvolume, values} p) . fromIntegral) [0 .. size - 1]
        return $ CVolume ints
    poke p (CVolume vols) = do
        #{poke struct pa_cvolume, channels} p $ (genericLength vols :: Word8)
        let indexed = zip [0..] vols
        mapM_ (uncurry (pokeElemOff (#{ptr struct pa_cvolume, values} p))) indexed


foreign import ccall "pa_sw_volume_to_linear" pa_sw_volume_to_linear :: Word32 -> Double

-- |Convert a single 'Volume' to a linear scale
volumeToLinear :: Word32 -> Double
volumeToLinear = pa_sw_volume_to_linear

-- |Convert a 'CVolume' to a list of linearly scaled values.
cVolumeToLinear :: CVolume -> [Double]
cVolumeToLinear (CVolume vols) = map volumeToLinear vols

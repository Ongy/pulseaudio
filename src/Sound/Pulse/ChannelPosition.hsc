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
Module      : sound.Pulse.ChannelPosition
Description : Provides types for PA_CHANNEL_POSITION and pa_channel_map.
Maintianer  : ongy
Stability   : experimental
-}
module Sound.Pulse.ChannelPosition
    ( ChannelPosition(..)
    , ChannelMap(..)
    )
where
#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif
#include <pulse/channelmap.h>

import Data.List (genericLength)
import Data.Word (Word8, Word)
import Foreign.Ptr (plusPtr) -- #{ptr ...} needs this
import Foreign.Storable (Storable(..))

import Sound.Pulse.Def (ChannelPosition(..), channelPositionFromInt, channelPositionToInt)

-- |The Type for ChannelMaps
newtype ChannelMap = ChannelMap [ChannelPosition] deriving (Eq, Show)

instance Storable ChannelMap where
    sizeOf _ = #{size struct pa_channel_map}
    alignment _ = #{alignment struct pa_channel_map}
    peek p = do
        size :: Word8 <- #{peek struct pa_channel_map, channels} p
        ints <- mapM (peekElemOff (#{ptr struct pa_channel_map, map} p) . fromIntegral ) [0..size - 1]
        return . ChannelMap $ map channelPositionFromInt ints
    poke p (ChannelMap pos) = do
        #{poke struct pa_channel_map, channels} p $ (genericLength pos :: Word8)
        let indexd = zip [0..] (map channelPositionToInt pos)
        mapM_ (uncurry (pokeElemOff (#{ptr struct pa_channel_map, map} p))) indexd

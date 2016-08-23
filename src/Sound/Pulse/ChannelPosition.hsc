{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Sound.Pulse.ChannelPosition
    ( ChannelPosition(..)
    , ChannelMap(..)
    )
where

#include <pulse/channelmap.h>

import Data.List (genericLength)
import Data.Word (Word8)
import Foreign.Ptr (plusPtr) -- #{ptr ...} needs this
import Foreign.Storable (Storable(..))

import Sound.Pulse.Def (ChannelPosition(..), channelPositionFromInt, channelPositionToInt)

newtype ChannelMap = ChannelMap [ChannelPosition] deriving (Eq, Show)

instance Storable ChannelMap where
    sizeOf _ = #{size struct pa_channel_map}
    alignment _ = alignment (undefined :: Word)
    peek p = do
        size :: Word8 <- #{peek struct pa_channel_map, channels} p
        ints <- mapM (peekElemOff (#{ptr struct pa_channel_map, map} p) . fromIntegral ) [0..size - 1]
        return . ChannelMap $ map channelPositionFromInt ints
    poke p (ChannelMap pos) = do
        #{poke struct pa_channel_map, channels} p $ (genericLength pos :: Word8)
        let indexd = zip [0..] (map channelPositionToInt pos)
        mapM_ (uncurry (pokeElemOff (#{ptr struct pa_channel_map, map} p))) indexd

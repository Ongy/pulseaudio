{-# LANGUAGE ScopedTypeVariables #-}
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

type Volume = Word32
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

volumeToLinear :: Word32 -> Double
volumeToLinear = pa_sw_volume_to_linear

cVolumeToLinear :: CVolume -> [Double]
cVolumeToLinear (CVolume vols) = map volumeToLinear vols

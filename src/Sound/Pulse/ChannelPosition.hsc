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
import Foreign.C.Types (CInt)
import Foreign.Ptr (plusPtr) -- #{ptr ...} needs this
import Foreign.Storable (Storable(..))

data ChannelPosition
    = ChannelPositionInvalid
    | ChannelPositionMono
    | ChannelPositionFrontLeft
    | ChannelPositionFrontRight
    | ChannelPositionFrontCenter
    | ChannelPositionLeft
    | ChannelPositionRight
    | ChannelPositionCenter
    | ChannelPositionRearCenter
    | ChannelPositionRearLeft
    | ChannelPositionRearRight
    | ChannelPositionLfe
    | ChannelPositionSubwoofer
    | ChannelPositionFrontLeftOfCenter
    | ChannelPositionFrontRightOfCenter
    | ChannelPositionSideLeft
    | ChannelPositionSideRight
    | ChannelPositionAux0
    | ChannelPositionAux1
    | ChannelPositionAux2
    | ChannelPositionAux3
    | ChannelPositionAux4
    | ChannelPositionAux5
    | ChannelPositionAux6
    | ChannelPositionAux7
    | ChannelPositionAux8
    | ChannelPositionAux9
    | ChannelPositionAux10
    | ChannelPositionAux11
    | ChannelPositionAux12
    | ChannelPositionAux13
    | ChannelPositionAux14
    | ChannelPositionAux15
    | ChannelPositionAux16
    | ChannelPositionAux17
    | ChannelPositionAux18
    | ChannelPositionAux19
    | ChannelPositionAux20
    | ChannelPositionAux21
    | ChannelPositionAux22
    | ChannelPositionAux23
    | ChannelPositionAux24
    | ChannelPositionAux25
    | ChannelPositionAux26
    | ChannelPositionAux27
    | ChannelPositionAux28
    | ChannelPositionAux29
    | ChannelPositionAux30
    | ChannelPositionAux31
    | ChannelPositionTopCenter
    | ChannelPositionTopFrontLeft
    | ChannelPositionTopFrontRight
    | ChannelPositionTopFrontCenter
    | ChannelPositionTopRearLeft
    | ChannelPositionTopRearRight
    | ChannelPositionTopRearCenter
    | ChannelPositionMax
    deriving (Eq, Show)

channelPositionToInt :: ChannelPosition -> CInt
channelPositionToInt ChannelPositionInvalid            = #{const PA_CHANNEL_POSITION_INVALID}
channelPositionToInt ChannelPositionMono               = #{const PA_CHANNEL_POSITION_MONO}
channelPositionToInt ChannelPositionFrontLeft          = #{const PA_CHANNEL_POSITION_FRONT_LEFT}
channelPositionToInt ChannelPositionFrontRight         = #{const PA_CHANNEL_POSITION_FRONT_RIGHT}
channelPositionToInt ChannelPositionFrontCenter        = #{const PA_CHANNEL_POSITION_FRONT_CENTER}
channelPositionToInt ChannelPositionLeft               = #{const PA_CHANNEL_POSITION_LEFT}
channelPositionToInt ChannelPositionRight              = #{const PA_CHANNEL_POSITION_RIGHT}
channelPositionToInt ChannelPositionCenter             = #{const PA_CHANNEL_POSITION_CENTER}
channelPositionToInt ChannelPositionRearCenter         = #{const PA_CHANNEL_POSITION_REAR_CENTER}
channelPositionToInt ChannelPositionRearLeft           = #{const PA_CHANNEL_POSITION_REAR_LEFT}
channelPositionToInt ChannelPositionRearRight          = #{const PA_CHANNEL_POSITION_REAR_RIGHT}
channelPositionToInt ChannelPositionLfe                = #{const PA_CHANNEL_POSITION_LFE}
channelPositionToInt ChannelPositionSubwoofer          = #{const PA_CHANNEL_POSITION_SUBWOOFER}
channelPositionToInt ChannelPositionFrontLeftOfCenter  = #{const PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER}
channelPositionToInt ChannelPositionFrontRightOfCenter = #{const PA_CHANNEL_POSITION_FRONT_RIGHT_OF_CENTER}
channelPositionToInt ChannelPositionSideLeft           = #{const PA_CHANNEL_POSITION_SIDE_LEFT}
channelPositionToInt ChannelPositionSideRight          = #{const PA_CHANNEL_POSITION_SIDE_RIGHT}
channelPositionToInt ChannelPositionAux0               = #{const PA_CHANNEL_POSITION_AUX0}
channelPositionToInt ChannelPositionAux1               = #{const PA_CHANNEL_POSITION_AUX1}
channelPositionToInt ChannelPositionAux2               = #{const PA_CHANNEL_POSITION_AUX2}
channelPositionToInt ChannelPositionAux3               = #{const PA_CHANNEL_POSITION_AUX3}
channelPositionToInt ChannelPositionAux4               = #{const PA_CHANNEL_POSITION_AUX4}
channelPositionToInt ChannelPositionAux5               = #{const PA_CHANNEL_POSITION_AUX5}
channelPositionToInt ChannelPositionAux6               = #{const PA_CHANNEL_POSITION_AUX6}
channelPositionToInt ChannelPositionAux7               = #{const PA_CHANNEL_POSITION_AUX7}
channelPositionToInt ChannelPositionAux8               = #{const PA_CHANNEL_POSITION_AUX8}
channelPositionToInt ChannelPositionAux9               = #{const PA_CHANNEL_POSITION_AUX9}
channelPositionToInt ChannelPositionAux10              = #{const PA_CHANNEL_POSITION_AUX10}
channelPositionToInt ChannelPositionAux11              = #{const PA_CHANNEL_POSITION_AUX11}
channelPositionToInt ChannelPositionAux12              = #{const PA_CHANNEL_POSITION_AUX12}
channelPositionToInt ChannelPositionAux13              = #{const PA_CHANNEL_POSITION_AUX13}
channelPositionToInt ChannelPositionAux14              = #{const PA_CHANNEL_POSITION_AUX14}
channelPositionToInt ChannelPositionAux15              = #{const PA_CHANNEL_POSITION_AUX15}
channelPositionToInt ChannelPositionAux16              = #{const PA_CHANNEL_POSITION_AUX16}
channelPositionToInt ChannelPositionAux17              = #{const PA_CHANNEL_POSITION_AUX17}
channelPositionToInt ChannelPositionAux18              = #{const PA_CHANNEL_POSITION_AUX18}
channelPositionToInt ChannelPositionAux19              = #{const PA_CHANNEL_POSITION_AUX19}
channelPositionToInt ChannelPositionAux20              = #{const PA_CHANNEL_POSITION_AUX20}
channelPositionToInt ChannelPositionAux21              = #{const PA_CHANNEL_POSITION_AUX21}
channelPositionToInt ChannelPositionAux22              = #{const PA_CHANNEL_POSITION_AUX22}
channelPositionToInt ChannelPositionAux23              = #{const PA_CHANNEL_POSITION_AUX23}
channelPositionToInt ChannelPositionAux24              = #{const PA_CHANNEL_POSITION_AUX24}
channelPositionToInt ChannelPositionAux25              = #{const PA_CHANNEL_POSITION_AUX25}
channelPositionToInt ChannelPositionAux26              = #{const PA_CHANNEL_POSITION_AUX26}
channelPositionToInt ChannelPositionAux27              = #{const PA_CHANNEL_POSITION_AUX27}
channelPositionToInt ChannelPositionAux28              = #{const PA_CHANNEL_POSITION_AUX28}
channelPositionToInt ChannelPositionAux29              = #{const PA_CHANNEL_POSITION_AUX29}
channelPositionToInt ChannelPositionAux30              = #{const PA_CHANNEL_POSITION_AUX30}
channelPositionToInt ChannelPositionAux31              = #{const PA_CHANNEL_POSITION_AUX31}
channelPositionToInt ChannelPositionTopCenter          = #{const PA_CHANNEL_POSITION_TOP_CENTER}
channelPositionToInt ChannelPositionTopFrontLeft       = #{const PA_CHANNEL_POSITION_TOP_FRONT_LEFT}
channelPositionToInt ChannelPositionTopFrontRight      = #{const PA_CHANNEL_POSITION_TOP_FRONT_RIGHT}
channelPositionToInt ChannelPositionTopFrontCenter     = #{const PA_CHANNEL_POSITION_TOP_FRONT_CENTER}
channelPositionToInt ChannelPositionTopRearLeft        = #{const PA_CHANNEL_POSITION_TOP_REAR_LEFT}
channelPositionToInt ChannelPositionTopRearRight       = #{const PA_CHANNEL_POSITION_TOP_REAR_RIGHT}
channelPositionToInt ChannelPositionTopRearCenter      = #{const PA_CHANNEL_POSITION_TOP_REAR_CENTER}
channelPositionToInt ChannelPositionMax                = #{const PA_CHANNEL_POSITION_MAX}

channelPositionFromInt :: CInt -> ChannelPosition
channelPositionFromInt i
    | i == #{const PA_CHANNEL_POSITION_INVALID}               = ChannelPositionInvalid
    | i == #{const PA_CHANNEL_POSITION_MONO}                  = ChannelPositionMono
    | i == #{const PA_CHANNEL_POSITION_FRONT_LEFT}            = ChannelPositionFrontLeft
    | i == #{const PA_CHANNEL_POSITION_FRONT_RIGHT}           = ChannelPositionFrontRight
    | i == #{const PA_CHANNEL_POSITION_FRONT_CENTER}          = ChannelPositionFrontCenter
    | i == #{const PA_CHANNEL_POSITION_LEFT}                  = ChannelPositionLeft
    | i == #{const PA_CHANNEL_POSITION_RIGHT}                 = ChannelPositionRight
    | i == #{const PA_CHANNEL_POSITION_CENTER}                = ChannelPositionCenter
    | i == #{const PA_CHANNEL_POSITION_REAR_CENTER}           = ChannelPositionRearCenter
    | i == #{const PA_CHANNEL_POSITION_REAR_LEFT}             = ChannelPositionRearLeft
    | i == #{const PA_CHANNEL_POSITION_REAR_RIGHT}            = ChannelPositionRearRight
    | i == #{const PA_CHANNEL_POSITION_LFE}                   = ChannelPositionLfe
    | i == #{const PA_CHANNEL_POSITION_SUBWOOFER}             = ChannelPositionSubwoofer
    | i == #{const PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER}  = ChannelPositionFrontLeftOfCenter
    | i == #{const PA_CHANNEL_POSITION_FRONT_RIGHT_OF_CENTER} = ChannelPositionFrontRightOfCenter
    | i == #{const PA_CHANNEL_POSITION_SIDE_LEFT}             = ChannelPositionSideLeft
    | i == #{const PA_CHANNEL_POSITION_SIDE_RIGHT}            = ChannelPositionSideRight
    | i == #{const PA_CHANNEL_POSITION_AUX0}                  = ChannelPositionAux0
    | i == #{const PA_CHANNEL_POSITION_AUX1}                  = ChannelPositionAux1
    | i == #{const PA_CHANNEL_POSITION_AUX2}                  = ChannelPositionAux2
    | i == #{const PA_CHANNEL_POSITION_AUX3}                  = ChannelPositionAux3
    | i == #{const PA_CHANNEL_POSITION_AUX4}                  = ChannelPositionAux4
    | i == #{const PA_CHANNEL_POSITION_AUX5}                  = ChannelPositionAux5
    | i == #{const PA_CHANNEL_POSITION_AUX6}                  = ChannelPositionAux6
    | i == #{const PA_CHANNEL_POSITION_AUX7}                  = ChannelPositionAux7
    | i == #{const PA_CHANNEL_POSITION_AUX8}                  = ChannelPositionAux8
    | i == #{const PA_CHANNEL_POSITION_AUX9}                  = ChannelPositionAux9
    | i == #{const PA_CHANNEL_POSITION_AUX10}                 = ChannelPositionAux10
    | i == #{const PA_CHANNEL_POSITION_AUX11}                 = ChannelPositionAux11
    | i == #{const PA_CHANNEL_POSITION_AUX12}                 = ChannelPositionAux12
    | i == #{const PA_CHANNEL_POSITION_AUX13}                 = ChannelPositionAux13
    | i == #{const PA_CHANNEL_POSITION_AUX14}                 = ChannelPositionAux14
    | i == #{const PA_CHANNEL_POSITION_AUX15}                 = ChannelPositionAux15
    | i == #{const PA_CHANNEL_POSITION_AUX16}                 = ChannelPositionAux16
    | i == #{const PA_CHANNEL_POSITION_AUX17}                 = ChannelPositionAux17
    | i == #{const PA_CHANNEL_POSITION_AUX18}                 = ChannelPositionAux18
    | i == #{const PA_CHANNEL_POSITION_AUX19}                 = ChannelPositionAux19
    | i == #{const PA_CHANNEL_POSITION_AUX20}                 = ChannelPositionAux20
    | i == #{const PA_CHANNEL_POSITION_AUX21}                 = ChannelPositionAux21
    | i == #{const PA_CHANNEL_POSITION_AUX22}                 = ChannelPositionAux22
    | i == #{const PA_CHANNEL_POSITION_AUX23}                 = ChannelPositionAux23
    | i == #{const PA_CHANNEL_POSITION_AUX24}                 = ChannelPositionAux24
    | i == #{const PA_CHANNEL_POSITION_AUX25}                 = ChannelPositionAux25
    | i == #{const PA_CHANNEL_POSITION_AUX26}                 = ChannelPositionAux26
    | i == #{const PA_CHANNEL_POSITION_AUX27}                 = ChannelPositionAux27
    | i == #{const PA_CHANNEL_POSITION_AUX28}                 = ChannelPositionAux28
    | i == #{const PA_CHANNEL_POSITION_AUX29}                 = ChannelPositionAux29
    | i == #{const PA_CHANNEL_POSITION_AUX30}                 = ChannelPositionAux30
    | i == #{const PA_CHANNEL_POSITION_AUX31}                 = ChannelPositionAux31
    | i == #{const PA_CHANNEL_POSITION_TOP_CENTER}            = ChannelPositionTopCenter
    | i == #{const PA_CHANNEL_POSITION_TOP_FRONT_LEFT}        = ChannelPositionTopFrontLeft
    | i == #{const PA_CHANNEL_POSITION_TOP_FRONT_RIGHT}       = ChannelPositionTopFrontRight
    | i == #{const PA_CHANNEL_POSITION_TOP_FRONT_CENTER}      = ChannelPositionTopFrontCenter
    | i == #{const PA_CHANNEL_POSITION_TOP_REAR_LEFT}         = ChannelPositionTopRearLeft
    | i == #{const PA_CHANNEL_POSITION_TOP_REAR_RIGHT}        = ChannelPositionTopRearRight
    | i == #{const PA_CHANNEL_POSITION_TOP_REAR_CENTER}       = ChannelPositionTopRearCenter
    | i == #{const PA_CHANNEL_POSITION_MAX}                   = ChannelPositionMax
    | otherwise = error "PA: found unexpcted channel position"


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

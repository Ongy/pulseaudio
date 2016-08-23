{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Data.Time.Internal
    ( PAITime
    , PATime(..)

    , toPAI
    , fromPAI
    )
where



#include <time.h>
#include <sys/time.h>

import Foreign.Storable
import Foreign.C.Types

-- Seconds and nanoseconds, compare with struct timespec (clock-gettime)
-- I'll make this Word Word, a few bytes more don't hurt that much
data PATime = PATime Word CLong deriving (Show, Eq, Ord)

instance Storable PATime where
    sizeOf _ = #{size struct timespec}
    alignment _ = alignment (undefined :: Word)
    peek p = PATime
        <$> #{peek struct timespec, tv_sec}  p
        <*> #{peek struct timespec, tv_nsec} p
    poke p (PATime sec nsec) = do
        #{poke struct timespec, tv_sec} p sec
        #{poke struct timespec, tv_nsec} p nsec


-- PAITime for internal stuff
data PAITime = PAITime Word CLong deriving (Show, Eq, Ord)

instance Storable PAITime where
    sizeOf _ = #{size struct timespec}
    alignment _ = alignment (undefined :: Word)
    peek p = PAITime
        <$> #{peek struct timeval, tv_sec}  p
        <*> #{peek struct timeval, tv_usec} p
    poke p (PAITime sec usec) = do
        #{poke struct timeval, tv_sec} p sec
        #{poke struct timeval, tv_usec} p usec

toPAI :: PATime -> PAITime
toPAI (PATime s ns) = PAITime s (ns `div` 1000)

fromPAI :: PAITime -> PATime
fromPAI (PAITime s ns) = PATime s (ns * 1000)

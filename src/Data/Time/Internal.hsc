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
{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-|
Module      : Data.Time.Internal
Description : Internal handling of time, and conversion between struct timeval and struct timespec
Maintianer  : ongy
Stability   : experimental
-}
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
-- |The time used by the library level api
data PATime = PATime Word CLong deriving (Show, Eq, Ord)

instance Storable PATime where
    sizeOf _ = #{size struct timespec}
    alignment _ = #{alignment struct timespec}
    peek p = PATime
        <$> #{peek struct timespec, tv_sec}  p
        <*> #{peek struct timespec, tv_nsec} p
    poke p (PATime sec nsec) = do
        #{poke struct timespec, tv_sec} p sec
        #{poke struct timespec, tv_nsec} p nsec


-- |Internal time struct used to convert to pulseaudio compatible format
data PAITime = PAITime Word CLong deriving (Show, Eq, Ord)

instance Storable PAITime where
    sizeOf _ = #{size struct timeval}
    alignment _ = #{alignment struct timeval}
    peek p = PAITime
        <$> #{peek struct timeval, tv_sec}  p
        <*> #{peek struct timeval, tv_usec} p
    poke p (PAITime sec usec) = do
        #{poke struct timeval, tv_sec} p sec
        #{poke struct timeval, tv_usec} p usec

-- |Convert from 'PATime' to 'PAITime' before passing to pulse
toPAI :: PATime -> PAITime
toPAI (PATime s ns) = PAITime s (ns `div` 1000)

-- |Convert from 'PAITime' to 'PATime' after getting value form pulse
fromPAI :: PAITime -> PATime
fromPAI (PAITime s ns) = PATime s (ns * 1000)

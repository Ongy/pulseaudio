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
{-# LANGUAGE EmptyDataDecls #-}
{-|
Module      : Sound.Pulse.Userdata
Description : provides a unifed Userdata type for this package.
Maintianer  : ongy
Stability   : experimental
-}
module Sound.Pulse.Userdata
    ( Userdata
    )
where

-- |This is the common Userdata type for pointers to userdata.
-- Userdata may either be something given to us by the pulse api
-- or a value passed from us to the pulse api.
-- Currently the only value passed to the pulse api, is the function pointer
-- for the defined function, so it can cleanup (freeHaskellFunPtr) itself.
data Userdata

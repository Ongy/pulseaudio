{-# LANGUAGE EmptyDataDecls #-}
module Sound.Pulse.Userdata
    ( Userdata
    )
where

-- |This is the common Userdata type for pointers to userdata, which we never
-- |use and always point to nullPtr.
-- |The userdata pointer is still passed through, since the C part of pulse
-- |uses it!
data Userdata

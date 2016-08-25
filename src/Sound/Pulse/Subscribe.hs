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
{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Sound.Pulse.Subscribe
Description : provides wrappers for subscription.
Maintianer  : ongy
Stability   : experimental
-}
module Sound.Pulse.Subscribe
    ( SubscriptionEventFacility(..)
    , SubscriptionEventType(..)
    , SubscriptionMask(..)

    , subscribeEvents
    )
where

import Sound.Pulse.Operation
import Data.Word (Word32, Word)
import Data.Bits ((.&.))
import Foreign.Ptr
import Foreign.C.Types
import Sound.Pulse.Context
import Sound.Pulse.Userdata
import Sound.Pulse.Def (SubscriptionEventFacility(..), SubscriptionEventType(..), subscriptionEventFacilityFromInt, subscriptionEventFacilityToInt, subscriptionEventTypeFromInt, subscriptionEventTypeToInt, SubscriptionMask(..), subscriptionMasksToInt)

subscriptionEventFromInt :: CInt -> (SubscriptionEventFacility, SubscriptionEventType)
subscriptionEventFromInt i =
    ( subscriptionEventFacilityFromInt (i .&. (subscriptionEventFacilityToInt SubscriptionEventFacilityMask))
    , subscriptionEventTypeFromInt (i .&. (subscriptionEventTypeToInt SubscriptionEventTypeMask) )
    )

type SubscribeCB = Context -> CInt -> CUInt -> Ptr Userdata -> IO ()
foreign import ccall "wrapper" mkSubscribeCB :: SubscribeCB -> IO (FunPtr SubscribeCB)

foreign import ccall "pa_context_set_subscribe_callback" pa_context_set_subscribe_callback
    :: Context
    -> FunPtr SubscribeCB
    -> Ptr Userdata
    -> IO ()

foreign import ccall "pa_context_subscribe" pa_context_subscribe
    :: Context
    -> CInt
    -> FunPtr ContextSuccessCB
    -> Ptr Userdata
    -> IO (Ptr UOperation)

-- |Currently the function given here is leaked! even if it's reset later on
-- This is currently unavoidable, since we don't know when the last event
-- occured.
-- This should only be called once per application run, so it will be a known
-- issue for now.
subscribeEvents
    :: Context -- ^The Context to subscribe on
    -> [SubscriptionMask] -- ^The events that should be reported
    -> ((SubscriptionEventFacility, SubscriptionEventType)
        -> Word32
        -> IO ()
       ) -- ^Callback function that will be called on every event received.
    -> IO Operation
subscribeEvents cxt mask fun = do
    funP <- mkSubscribeCB $ \_ ival idx _ ->
        fun (subscriptionEventFromInt ival) (fromIntegral idx)
    pa_context_set_subscribe_callback cxt funP nullPtr
    sucP <- wrapSuccess (\b -> putStrLn ("Subscription success: " ++ show b))
    ptrToOperation =<< pa_context_subscribe cxt (subscriptionMasksToInt mask) sucP nullPtr

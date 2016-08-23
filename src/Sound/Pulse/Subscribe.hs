{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.Pulse.Subscribe
    ( SubscriptionEventFacility(..)
    , SubscriptionEventType(..)
    , SubscriptionMask(..)

    , subscribeEvents
    )
where

-- ToInts commented out, because they will never be used (for now)
#include <pulse/subscribe.h>

import Sound.Pulse.Operation
import Data.Word (Word32)
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

subscribeEvents
    :: Context
    -> [SubscriptionMask]
    -> ((SubscriptionEventFacility, SubscriptionEventType)
        -> Word32
        -> IO ()
        )
    -> IO Operation
subscribeEvents cxt mask fun = do
    funP <- mkSubscribeCB $ \_ ival idx _ ->
        fun (subscriptionEventFromInt ival) (fromIntegral idx)
    pa_context_set_subscribe_callback cxt funP nullPtr
    sucP <- wrapSuccess (\b -> putStrLn ("Subscription success: " ++ show b))
    ptrToOperation =<< pa_context_subscribe cxt (subscriptionMasksToInt mask) sucP nullPtr

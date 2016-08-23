{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Sound.Pulse.Subscribe
    ( SubscriptionEventFacility(..)
    , SubscriptionEventType(..)
    , SubscriptionMask(..)

    , subscribeEvents
    )
where

-- ToInts commented out, because they will never be used (for now)
#include <pulse/subscribe.h>
#include <pulse/def.h>
import Sound.Pulse.Operation
import Data.Word (Word32)
import Data.Bits ((.|.), (.&.))
import Foreign.Ptr
import Foreign.C.Types
import Sound.Pulse.Context
import Sound.Pulse.Userdata

data SubscriptionEventFacility
    = SubscriptionEventSink
    | SubscriptionEventSource
    | SubscriptionEventSinkInput
    | SubscriptionEventSourceOutput
    | SubscriptionEventModule
    | SubscriptionEventClient
    | SubscriptionEventSampleCache
    | SubscriptionEventServer
    | SubscriptionEventCard
    deriving (Eq, Show)
      --- | SubscriptionEventFacilityMask

data SubscriptionEventType
    = SubscriptionEventNew
    | SubscriptionEventChange
    | SubscriptionEventRemove
    deriving (Eq, Show)
      --- | SubscriptionEventTypeMask

-- subscriptionEventFacilityToInt :: SubscriptionEventFacility -> CInt
-- subscriptionEventFacilityToInt SubscriptionEventSink         = #{const PA_SUBSCRIPTION_EVENT_SINK}
-- subscriptionEventFacilityToInt SubscriptionEventSource       = #{const PA_SUBSCRIPTION_EVENT_SOURCE}
-- subscriptionEventFacilityToInt SubscriptionEventSinkInput    = #{const PA_SUBSCRIPTION_EVENT_SINK_INPUT}
-- subscriptionEventFacilityToInt SubscriptionEventSourceOutput = #{const PA_SUBSCRIPTION_EVENT_SOURCE_OUTPUT}
-- subscriptionEventFacilityToInt SubscriptionEventModule       = #{const PA_SUBSCRIPTION_EVENT_MODULE}
-- subscriptionEventFacilityToInt SubscriptionEventClient       = #{const PA_SUBSCRIPTION_EVENT_CLIENT}
-- subscriptionEventFacilityToInt SubscriptionEventSampleCache  = #{const PA_SUBSCRIPTION_EVENT_SAMPLE_CACHE}
-- subscriptionEventFacilityToInt SubscriptionEventServer       = #{const PA_SUBSCRIPTION_EVENT_SERVER}
-- subscriptionEventFacilityToInt SubscriptionEventCard         = #{const PA_SUBSCRIPTION_EVENT_CARD}

subscriptionEventFacilityFromInt ::CInt -> SubscriptionEventFacility
subscriptionEventFacilityFromInt i
    | i == #{const PA_SUBSCRIPTION_EVENT_SINK}          = SubscriptionEventSink
    | i == #{const PA_SUBSCRIPTION_EVENT_SOURCE}        = SubscriptionEventSource
    | i == #{const PA_SUBSCRIPTION_EVENT_SINK_INPUT}    = SubscriptionEventSinkInput
    | i == #{const PA_SUBSCRIPTION_EVENT_SOURCE_OUTPUT} = SubscriptionEventSourceOutput
    | i == #{const PA_SUBSCRIPTION_EVENT_MODULE}        = SubscriptionEventModule
    | i == #{const PA_SUBSCRIPTION_EVENT_CLIENT}        = SubscriptionEventClient
    | i == #{const PA_SUBSCRIPTION_EVENT_SAMPLE_CACHE}  = SubscriptionEventSampleCache
    | i == #{const PA_SUBSCRIPTION_EVENT_SERVER}        = SubscriptionEventServer
    | i == #{const PA_SUBSCRIPTION_EVENT_CARD}          = SubscriptionEventCard
    | otherwise = error "PA: Unexpted subscription event facility"

-- subscriptionEvenTypeToInt :: SubscriptionEventType -> CInt
-- subscriptionEvenTypeToInt SubscriptionEventNew    = #{const PA_SUBSCRIPTION_EVENT_NEW}
-- subscriptionEvenTypeToInt SubscriptionEventChange = #{const PA_SUBSCRIPTION_EVENT_CHANGE}
-- subscriptionEvenTypeToInt SubscriptionEventRemove = #{const PA_SUBSCRIPTION_EVENT_REMOVE}

subscriptionEventTypeFromInt :: CInt -> SubscriptionEventType
subscriptionEventTypeFromInt i
    | i == #{const PA_SUBSCRIPTION_EVENT_NEW}    = SubscriptionEventNew
    | i == #{const PA_SUBSCRIPTION_EVENT_CHANGE} = SubscriptionEventChange
    | i == #{const PA_SUBSCRIPTION_EVENT_REMOVE} = SubscriptionEventRemove
    | otherwise = error "PA: Unexpected subscription even type"

-- subscriptionEventToInt :: (SubscriptionEventFacility, SubscriptionEventType) -> CInt
-- subscriptionEventToInt (x, y) =
--     subscriptionEventFacilityToInt x .|. subscriptionEvenTypeToInt y

subscriptionEventFromInt :: CInt -> (SubscriptionEventFacility, SubscriptionEventType)
subscriptionEventFromInt i =
    ( subscriptionEventFacilityFromInt (i .&. #{const PA_SUBSCRIPTION_EVENT_FACILITY_MASK})
    , subscriptionEventTypeFromInt (i .&. #{const PA_SUBSCRIPTION_EVENT_TYPE_MASK})
    )

-- TODO :)
data SubscriptionMask
    = SubscriptionMaskSink
    | SubscriptionMaskSource
    | SubscriptionMaskSinkInput
    | SubscriptionMaskSourceOutput
    | SubscriptionMaskModule
    | SubscriptionMaskClient
    | SubscriptionMaskSampleCache
    | SubscriptionMaskServer
    | SubscriptionMaskCard
    | SubscriptionMaskAll
    deriving (Eq, Show)

subscriptionMaskToInt :: SubscriptionMask -> CInt
subscriptionMaskToInt SubscriptionMaskSink         = #{const PA_SUBSCRIPTION_MASK_SINK}
subscriptionMaskToInt SubscriptionMaskSource       = #{const PA_SUBSCRIPTION_MASK_SOURCE}
subscriptionMaskToInt SubscriptionMaskSinkInput    = #{const PA_SUBSCRIPTION_MASK_SINK_INPUT}
subscriptionMaskToInt SubscriptionMaskSourceOutput = #{const PA_SUBSCRIPTION_MASK_SOURCE_OUTPUT}
subscriptionMaskToInt SubscriptionMaskModule       = #{const PA_SUBSCRIPTION_MASK_MODULE}
subscriptionMaskToInt SubscriptionMaskClient       = #{const PA_SUBSCRIPTION_MASK_CLIENT}
subscriptionMaskToInt SubscriptionMaskSampleCache  = #{const PA_SUBSCRIPTION_MASK_SAMPLE_CACHE}
subscriptionMaskToInt SubscriptionMaskServer       = #{const PA_SUBSCRIPTION_MASK_SERVER}
subscriptionMaskToInt SubscriptionMaskCard         = #{const PA_SUBSCRIPTION_MASK_CARD}
subscriptionMaskToInt SubscriptionMaskAll          = #{const PA_SUBSCRIPTION_MASK_ALL}

subscriptionMasksToInt :: [SubscriptionMask] -> CInt
subscriptionMasksToInt = foldr (.|.) 0 . map subscriptionMaskToInt

type SubscribeCB = PAContext -> CInt -> CUInt -> Ptr Userdata -> IO ()
foreign import ccall "wrapper" mkSubscribeCB :: SubscribeCB -> IO (FunPtr SubscribeCB)

-- pa_operation* pa_context_subscribe(pa_context_subscribepa_context *  c,
-- pa_subscription_mask_t   m,
-- pa_context_success_cb_t  cb,
-- void *   userdata 
-- )userdata


foreign import ccall "pa_context_set_subscribe_callback" pa_context_set_subscribe_callback
    :: PAContext
    -> FunPtr SubscribeCB
    -> Ptr Userdata
    -> IO ()

foreign import ccall "pa_context_subscribe" pa_context_subscribe
    :: PAContext
    -> CInt
    -> FunPtr PAContextSuccessCB
    -> Ptr Userdata
    -> IO (Ptr UOperation)

subscribeEvents
    :: PAContext
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

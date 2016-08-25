{-# OPTIONS -fno-warn-overlapping-patterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Sound.Pulse.Def
where

#include <pulse/def.h>
#include <pulse/channelmap.h>
#include <pulse/sample.h>
import Data.Bits (Bits(..))
import Foreign.C.Types (CInt)

foldFlag :: (a -> CInt) -> [a] -> CInt
foldFlag fun = foldr ((.|.) . fun) 0

data SubscriptionEventFacility
    = SubscriptionEventSink
    | SubscriptionEventSource
    | SubscriptionEventSinkInput
    | SubscriptionEventSourceOutput
    | SubscriptionEventModule
    | SubscriptionEventClient
    | SubscriptionEventSampleCache
    | SubscriptionEventServer
    | SubscriptionEventAutoload
    | SubscriptionEventCard
    | SubscriptionEventFacilityMask
    deriving (Eq, Show)

subscriptionEventFacilityToInt :: SubscriptionEventFacility -> CInt
subscriptionEventFacilityToInt SubscriptionEventSink = #{const PA_SUBSCRIPTION_EVENT_SINK}
subscriptionEventFacilityToInt SubscriptionEventSource = #{const PA_SUBSCRIPTION_EVENT_SOURCE}
subscriptionEventFacilityToInt SubscriptionEventSinkInput = #{const PA_SUBSCRIPTION_EVENT_SINK_INPUT}
subscriptionEventFacilityToInt SubscriptionEventSourceOutput = #{const PA_SUBSCRIPTION_EVENT_SOURCE_OUTPUT}
subscriptionEventFacilityToInt SubscriptionEventModule = #{const PA_SUBSCRIPTION_EVENT_MODULE}
subscriptionEventFacilityToInt SubscriptionEventClient = #{const PA_SUBSCRIPTION_EVENT_CLIENT}
subscriptionEventFacilityToInt SubscriptionEventSampleCache = #{const PA_SUBSCRIPTION_EVENT_SAMPLE_CACHE}
subscriptionEventFacilityToInt SubscriptionEventServer = #{const PA_SUBSCRIPTION_EVENT_SERVER}
subscriptionEventFacilityToInt SubscriptionEventAutoload = #{const PA_SUBSCRIPTION_EVENT_AUTOLOAD}
subscriptionEventFacilityToInt SubscriptionEventCard = #{const PA_SUBSCRIPTION_EVENT_CARD}
subscriptionEventFacilityToInt SubscriptionEventFacilityMask = #{const PA_SUBSCRIPTION_EVENT_FACILITY_MASK}

subscriptionEventFacilityFromInt :: CInt -> SubscriptionEventFacility
subscriptionEventFacilityFromInt (#{const PA_SUBSCRIPTION_EVENT_SINK}) = SubscriptionEventSink
subscriptionEventFacilityFromInt (#{const PA_SUBSCRIPTION_EVENT_SOURCE}) = SubscriptionEventSource
subscriptionEventFacilityFromInt (#{const PA_SUBSCRIPTION_EVENT_SINK_INPUT}) = SubscriptionEventSinkInput
subscriptionEventFacilityFromInt (#{const PA_SUBSCRIPTION_EVENT_SOURCE_OUTPUT}) = SubscriptionEventSourceOutput
subscriptionEventFacilityFromInt (#{const PA_SUBSCRIPTION_EVENT_MODULE}) = SubscriptionEventModule
subscriptionEventFacilityFromInt (#{const PA_SUBSCRIPTION_EVENT_CLIENT}) = SubscriptionEventClient
subscriptionEventFacilityFromInt (#{const PA_SUBSCRIPTION_EVENT_SAMPLE_CACHE}) = SubscriptionEventSampleCache
subscriptionEventFacilityFromInt (#{const PA_SUBSCRIPTION_EVENT_SERVER}) = SubscriptionEventServer
subscriptionEventFacilityFromInt (#{const PA_SUBSCRIPTION_EVENT_AUTOLOAD}) = SubscriptionEventAutoload
subscriptionEventFacilityFromInt (#{const PA_SUBSCRIPTION_EVENT_CARD}) = SubscriptionEventCard
subscriptionEventFacilityFromInt (#{const PA_SUBSCRIPTION_EVENT_FACILITY_MASK}) = SubscriptionEventFacilityMask
subscriptionEventFacilityFromInt x = error ("PA unexped value @subscriptionEventFacilityFromInt:" ++ show x)
data SubscriptionEventType
    = SubscriptionEventNew
    | SubscriptionEventChange
    | SubscriptionEventRemove
    | SubscriptionEventTypeMask
    deriving (Eq, Show)

subscriptionEventTypeToInt :: SubscriptionEventType -> CInt
subscriptionEventTypeToInt SubscriptionEventNew = #{const PA_SUBSCRIPTION_EVENT_NEW}
subscriptionEventTypeToInt SubscriptionEventChange = #{const PA_SUBSCRIPTION_EVENT_CHANGE}
subscriptionEventTypeToInt SubscriptionEventRemove = #{const PA_SUBSCRIPTION_EVENT_REMOVE}
subscriptionEventTypeToInt SubscriptionEventTypeMask = #{const PA_SUBSCRIPTION_EVENT_TYPE_MASK}

subscriptionEventTypeFromInt :: CInt -> SubscriptionEventType
subscriptionEventTypeFromInt (#{const PA_SUBSCRIPTION_EVENT_NEW}) = SubscriptionEventNew
subscriptionEventTypeFromInt (#{const PA_SUBSCRIPTION_EVENT_CHANGE}) = SubscriptionEventChange
subscriptionEventTypeFromInt (#{const PA_SUBSCRIPTION_EVENT_REMOVE}) = SubscriptionEventRemove
subscriptionEventTypeFromInt (#{const PA_SUBSCRIPTION_EVENT_TYPE_MASK}) = SubscriptionEventTypeMask
subscriptionEventTypeFromInt x = error ("PA unexped value @subscriptionEventTypeFromInt:" ++ show x)
data ContextState
    = ContextUnconnected
    | ContextConnecting
    | ContextAuthorizing
    | ContextSettingName
    | ContextReady
    | ContextFailed
    | ContextTerminated
    deriving (Eq, Show)

contextStateToInt :: ContextState -> CInt
contextStateToInt ContextUnconnected = #{const PA_CONTEXT_UNCONNECTED}
contextStateToInt ContextConnecting = #{const PA_CONTEXT_CONNECTING}
contextStateToInt ContextAuthorizing = #{const PA_CONTEXT_AUTHORIZING}
contextStateToInt ContextSettingName = #{const PA_CONTEXT_SETTING_NAME}
contextStateToInt ContextReady = #{const PA_CONTEXT_READY}
contextStateToInt ContextFailed = #{const PA_CONTEXT_FAILED}
contextStateToInt ContextTerminated = #{const PA_CONTEXT_TERMINATED}

contextStateFromInt :: CInt -> ContextState
contextStateFromInt (#{const PA_CONTEXT_UNCONNECTED}) = ContextUnconnected
contextStateFromInt (#{const PA_CONTEXT_CONNECTING}) = ContextConnecting
contextStateFromInt (#{const PA_CONTEXT_AUTHORIZING}) = ContextAuthorizing
contextStateFromInt (#{const PA_CONTEXT_SETTING_NAME}) = ContextSettingName
contextStateFromInt (#{const PA_CONTEXT_READY}) = ContextReady
contextStateFromInt (#{const PA_CONTEXT_FAILED}) = ContextFailed
contextStateFromInt (#{const PA_CONTEXT_TERMINATED}) = ContextTerminated
contextStateFromInt x = error ("PA unexped value @contextStateFromInt:" ++ show x)
data StreamState
    = StreamUnconnected
    | StreamCreating
    | StreamReady
    | StreamFailed
    | StreamTerminated
    deriving (Eq, Show)

streamStateToInt :: StreamState -> CInt
streamStateToInt StreamUnconnected = #{const PA_STREAM_UNCONNECTED}
streamStateToInt StreamCreating = #{const PA_STREAM_CREATING}
streamStateToInt StreamReady = #{const PA_STREAM_READY}
streamStateToInt StreamFailed = #{const PA_STREAM_FAILED}
streamStateToInt StreamTerminated = #{const PA_STREAM_TERMINATED}

streamStateFromInt :: CInt -> StreamState
streamStateFromInt (#{const PA_STREAM_UNCONNECTED}) = StreamUnconnected
streamStateFromInt (#{const PA_STREAM_CREATING}) = StreamCreating
streamStateFromInt (#{const PA_STREAM_READY}) = StreamReady
streamStateFromInt (#{const PA_STREAM_FAILED}) = StreamFailed
streamStateFromInt (#{const PA_STREAM_TERMINATED}) = StreamTerminated
streamStateFromInt x = error ("PA unexped value @streamStateFromInt:" ++ show x)
data OperationState
    = OperationRunning
    | OperationDone
    | OperationCancelled
    deriving (Eq, Show)

operationStateToInt :: OperationState -> CInt
operationStateToInt OperationRunning = #{const PA_OPERATION_RUNNING}
operationStateToInt OperationDone = #{const PA_OPERATION_DONE}
operationStateToInt OperationCancelled = #{const PA_OPERATION_CANCELLED}

operationStateFromInt :: CInt -> OperationState
operationStateFromInt (#{const PA_OPERATION_RUNNING}) = OperationRunning
operationStateFromInt (#{const PA_OPERATION_DONE}) = OperationDone
operationStateFromInt (#{const PA_OPERATION_CANCELLED}) = OperationCancelled
operationStateFromInt x = error ("PA unexped value @operationStateFromInt:" ++ show x)
data Direction
    = DirectionOutput
    | DirectionInput
    deriving (Eq, Show)

directionToInt :: Direction -> CInt
directionToInt DirectionOutput = #{const PA_DIRECTION_OUTPUT}
directionToInt DirectionInput = #{const PA_DIRECTION_INPUT}

directionFromInt :: CInt -> Direction
directionFromInt (#{const PA_DIRECTION_OUTPUT}) = DirectionOutput
directionFromInt (#{const PA_DIRECTION_INPUT}) = DirectionInput
directionFromInt x = error ("PA unexped value @directionFromInt:" ++ show x)
data ErrorCode
    = Ok
    | ErrAccess
    | ErrCommand
    | ErrInvalid
    | ErrExist
    | ErrNoentity
    | ErrConnectionrefused
    | ErrProtocol
    | ErrTimeout
    | ErrAuthkey
    | ErrInternal
    | ErrConnectionterminated
    | ErrKilled
    | ErrInvalidserver
    | ErrModinitfailed
    | ErrBadstate
    | ErrNodata
    | ErrVersion
    | ErrToolarge
    | ErrNotsupported
    | ErrUnknown
    | ErrNoextension
    | ErrObsolete
    | ErrNotimplemented
    | ErrForked
    | ErrIo
    | ErrBusy
    | ErrMax
    deriving (Eq, Show)

errorCodeToInt :: ErrorCode -> CInt
errorCodeToInt Ok = #{const PA_OK}
errorCodeToInt ErrAccess = #{const PA_ERR_ACCESS}
errorCodeToInt ErrCommand = #{const PA_ERR_COMMAND}
errorCodeToInt ErrInvalid = #{const PA_ERR_INVALID}
errorCodeToInt ErrExist = #{const PA_ERR_EXIST}
errorCodeToInt ErrNoentity = #{const PA_ERR_NOENTITY}
errorCodeToInt ErrConnectionrefused = #{const PA_ERR_CONNECTIONREFUSED}
errorCodeToInt ErrProtocol = #{const PA_ERR_PROTOCOL}
errorCodeToInt ErrTimeout = #{const PA_ERR_TIMEOUT}
errorCodeToInt ErrAuthkey = #{const PA_ERR_AUTHKEY}
errorCodeToInt ErrInternal = #{const PA_ERR_INTERNAL}
errorCodeToInt ErrConnectionterminated = #{const PA_ERR_CONNECTIONTERMINATED}
errorCodeToInt ErrKilled = #{const PA_ERR_KILLED}
errorCodeToInt ErrInvalidserver = #{const PA_ERR_INVALIDSERVER}
errorCodeToInt ErrModinitfailed = #{const PA_ERR_MODINITFAILED}
errorCodeToInt ErrBadstate = #{const PA_ERR_BADSTATE}
errorCodeToInt ErrNodata = #{const PA_ERR_NODATA}
errorCodeToInt ErrVersion = #{const PA_ERR_VERSION}
errorCodeToInt ErrToolarge = #{const PA_ERR_TOOLARGE}
errorCodeToInt ErrNotsupported = #{const PA_ERR_NOTSUPPORTED}
errorCodeToInt ErrUnknown = #{const PA_ERR_UNKNOWN}
errorCodeToInt ErrNoextension = #{const PA_ERR_NOEXTENSION}
errorCodeToInt ErrObsolete = #{const PA_ERR_OBSOLETE}
errorCodeToInt ErrNotimplemented = #{const PA_ERR_NOTIMPLEMENTED}
errorCodeToInt ErrForked = #{const PA_ERR_FORKED}
errorCodeToInt ErrIo = #{const PA_ERR_IO}
errorCodeToInt ErrBusy = #{const PA_ERR_BUSY}
errorCodeToInt ErrMax = #{const PA_ERR_MAX}

errorCodeFromInt :: CInt -> ErrorCode
errorCodeFromInt (#{const PA_OK}) = Ok
errorCodeFromInt (#{const PA_ERR_ACCESS}) = ErrAccess
errorCodeFromInt (#{const PA_ERR_COMMAND}) = ErrCommand
errorCodeFromInt (#{const PA_ERR_INVALID}) = ErrInvalid
errorCodeFromInt (#{const PA_ERR_EXIST}) = ErrExist
errorCodeFromInt (#{const PA_ERR_NOENTITY}) = ErrNoentity
errorCodeFromInt (#{const PA_ERR_CONNECTIONREFUSED}) = ErrConnectionrefused
errorCodeFromInt (#{const PA_ERR_PROTOCOL}) = ErrProtocol
errorCodeFromInt (#{const PA_ERR_TIMEOUT}) = ErrTimeout
errorCodeFromInt (#{const PA_ERR_AUTHKEY}) = ErrAuthkey
errorCodeFromInt (#{const PA_ERR_INTERNAL}) = ErrInternal
errorCodeFromInt (#{const PA_ERR_CONNECTIONTERMINATED}) = ErrConnectionterminated
errorCodeFromInt (#{const PA_ERR_KILLED}) = ErrKilled
errorCodeFromInt (#{const PA_ERR_INVALIDSERVER}) = ErrInvalidserver
errorCodeFromInt (#{const PA_ERR_MODINITFAILED}) = ErrModinitfailed
errorCodeFromInt (#{const PA_ERR_BADSTATE}) = ErrBadstate
errorCodeFromInt (#{const PA_ERR_NODATA}) = ErrNodata
errorCodeFromInt (#{const PA_ERR_VERSION}) = ErrVersion
errorCodeFromInt (#{const PA_ERR_TOOLARGE}) = ErrToolarge
errorCodeFromInt (#{const PA_ERR_NOTSUPPORTED}) = ErrNotsupported
errorCodeFromInt (#{const PA_ERR_UNKNOWN}) = ErrUnknown
errorCodeFromInt (#{const PA_ERR_NOEXTENSION}) = ErrNoextension
errorCodeFromInt (#{const PA_ERR_OBSOLETE}) = ErrObsolete
errorCodeFromInt (#{const PA_ERR_NOTIMPLEMENTED}) = ErrNotimplemented
errorCodeFromInt (#{const PA_ERR_FORKED}) = ErrForked
errorCodeFromInt (#{const PA_ERR_IO}) = ErrIo
errorCodeFromInt (#{const PA_ERR_BUSY}) = ErrBusy
errorCodeFromInt (#{const PA_ERR_MAX}) = ErrMax
errorCodeFromInt x = error ("PA unexped value @errorCodeFromInt:" ++ show x)
data SeekMode
    = SeekRelative
    | SeekAbsolute
    | SeekRelativeOnRead
    | SeekRelativeEnd
    deriving (Eq, Show)

seekModeToInt :: SeekMode -> CInt
seekModeToInt SeekRelative = #{const PA_SEEK_RELATIVE}
seekModeToInt SeekAbsolute = #{const PA_SEEK_ABSOLUTE}
seekModeToInt SeekRelativeOnRead = #{const PA_SEEK_RELATIVE_ON_READ}
seekModeToInt SeekRelativeEnd = #{const PA_SEEK_RELATIVE_END}

seekModeFromInt :: CInt -> SeekMode
seekModeFromInt (#{const PA_SEEK_RELATIVE}) = SeekRelative
seekModeFromInt (#{const PA_SEEK_ABSOLUTE}) = SeekAbsolute
seekModeFromInt (#{const PA_SEEK_RELATIVE_ON_READ}) = SeekRelativeOnRead
seekModeFromInt (#{const PA_SEEK_RELATIVE_END}) = SeekRelativeEnd
seekModeFromInt x = error ("PA unexped value @seekModeFromInt:" ++ show x)
data SinkState
    = SinkInvalidState
    | SinkRunning
    | SinkIdle
    | SinkSuspended
    | SinkInit
    | SinkUnlinked
    deriving (Eq, Show)

sinkStateToInt :: SinkState -> CInt
sinkStateToInt SinkInvalidState = #{const PA_SINK_INVALID_STATE}
sinkStateToInt SinkRunning = #{const PA_SINK_RUNNING}
sinkStateToInt SinkIdle = #{const PA_SINK_IDLE}
sinkStateToInt SinkSuspended = #{const PA_SINK_SUSPENDED}
sinkStateToInt SinkInit = #{const PA_SINK_INIT}
sinkStateToInt SinkUnlinked = #{const PA_SINK_UNLINKED}

sinkStateFromInt :: CInt -> SinkState
sinkStateFromInt (#{const PA_SINK_INVALID_STATE}) = SinkInvalidState
sinkStateFromInt (#{const PA_SINK_RUNNING}) = SinkRunning
sinkStateFromInt (#{const PA_SINK_IDLE}) = SinkIdle
sinkStateFromInt (#{const PA_SINK_SUSPENDED}) = SinkSuspended
sinkStateFromInt (#{const PA_SINK_INIT}) = SinkInit
sinkStateFromInt (#{const PA_SINK_UNLINKED}) = SinkUnlinked
sinkStateFromInt x = error ("PA unexped value @sinkStateFromInt:" ++ show x)
data SourceState
    = SourceInvalidState
    | SourceRunning
    | SourceIdle
    | SourceSuspended
    | SourceInit
    | SourceUnlinked
    deriving (Eq, Show)

sourceStateToInt :: SourceState -> CInt
sourceStateToInt SourceInvalidState = #{const PA_SOURCE_INVALID_STATE}
sourceStateToInt SourceRunning = #{const PA_SOURCE_RUNNING}
sourceStateToInt SourceIdle = #{const PA_SOURCE_IDLE}
sourceStateToInt SourceSuspended = #{const PA_SOURCE_SUSPENDED}
sourceStateToInt SourceInit = #{const PA_SOURCE_INIT}
sourceStateToInt SourceUnlinked = #{const PA_SOURCE_UNLINKED}

sourceStateFromInt :: CInt -> SourceState
sourceStateFromInt (#{const PA_SOURCE_INVALID_STATE}) = SourceInvalidState
sourceStateFromInt (#{const PA_SOURCE_RUNNING}) = SourceRunning
sourceStateFromInt (#{const PA_SOURCE_IDLE}) = SourceIdle
sourceStateFromInt (#{const PA_SOURCE_SUSPENDED}) = SourceSuspended
sourceStateFromInt (#{const PA_SOURCE_INIT}) = SourceInit
sourceStateFromInt (#{const PA_SOURCE_UNLINKED}) = SourceUnlinked
sourceStateFromInt x = error ("PA unexped value @sourceStateFromInt:" ++ show x)
data PortAvailable
    = PortAvailableUnknown
    | PortAvailableNo
    | PortAvailableYes
    deriving (Eq, Show)

portAvailableToInt :: PortAvailable -> CInt
portAvailableToInt PortAvailableUnknown = #{const PA_PORT_AVAILABLE_UNKNOWN}
portAvailableToInt PortAvailableNo = #{const PA_PORT_AVAILABLE_NO}
portAvailableToInt PortAvailableYes = #{const PA_PORT_AVAILABLE_YES}

portAvailableFromInt :: CInt -> PortAvailable
portAvailableFromInt (#{const PA_PORT_AVAILABLE_UNKNOWN}) = PortAvailableUnknown
portAvailableFromInt (#{const PA_PORT_AVAILABLE_NO}) = PortAvailableNo
portAvailableFromInt (#{const PA_PORT_AVAILABLE_YES}) = PortAvailableYes
portAvailableFromInt x = error ("PA unexped value @portAvailableFromInt:" ++ show x)
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
channelPositionToInt ChannelPositionInvalid = #{const PA_CHANNEL_POSITION_INVALID}
channelPositionToInt ChannelPositionMono = #{const PA_CHANNEL_POSITION_MONO}
channelPositionToInt ChannelPositionFrontLeft = #{const PA_CHANNEL_POSITION_FRONT_LEFT}
channelPositionToInt ChannelPositionFrontRight = #{const PA_CHANNEL_POSITION_FRONT_RIGHT}
channelPositionToInt ChannelPositionFrontCenter = #{const PA_CHANNEL_POSITION_FRONT_CENTER}
channelPositionToInt ChannelPositionLeft = #{const PA_CHANNEL_POSITION_LEFT}
channelPositionToInt ChannelPositionRight = #{const PA_CHANNEL_POSITION_RIGHT}
channelPositionToInt ChannelPositionCenter = #{const PA_CHANNEL_POSITION_CENTER}
channelPositionToInt ChannelPositionRearCenter = #{const PA_CHANNEL_POSITION_REAR_CENTER}
channelPositionToInt ChannelPositionRearLeft = #{const PA_CHANNEL_POSITION_REAR_LEFT}
channelPositionToInt ChannelPositionRearRight = #{const PA_CHANNEL_POSITION_REAR_RIGHT}
channelPositionToInt ChannelPositionLfe = #{const PA_CHANNEL_POSITION_LFE}
channelPositionToInt ChannelPositionSubwoofer = #{const PA_CHANNEL_POSITION_SUBWOOFER}
channelPositionToInt ChannelPositionFrontLeftOfCenter = #{const PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER}
channelPositionToInt ChannelPositionFrontRightOfCenter = #{const PA_CHANNEL_POSITION_FRONT_RIGHT_OF_CENTER}
channelPositionToInt ChannelPositionSideLeft = #{const PA_CHANNEL_POSITION_SIDE_LEFT}
channelPositionToInt ChannelPositionSideRight = #{const PA_CHANNEL_POSITION_SIDE_RIGHT}
channelPositionToInt ChannelPositionAux0 = #{const PA_CHANNEL_POSITION_AUX0}
channelPositionToInt ChannelPositionAux1 = #{const PA_CHANNEL_POSITION_AUX1}
channelPositionToInt ChannelPositionAux2 = #{const PA_CHANNEL_POSITION_AUX2}
channelPositionToInt ChannelPositionAux3 = #{const PA_CHANNEL_POSITION_AUX3}
channelPositionToInt ChannelPositionAux4 = #{const PA_CHANNEL_POSITION_AUX4}
channelPositionToInt ChannelPositionAux5 = #{const PA_CHANNEL_POSITION_AUX5}
channelPositionToInt ChannelPositionAux6 = #{const PA_CHANNEL_POSITION_AUX6}
channelPositionToInt ChannelPositionAux7 = #{const PA_CHANNEL_POSITION_AUX7}
channelPositionToInt ChannelPositionAux8 = #{const PA_CHANNEL_POSITION_AUX8}
channelPositionToInt ChannelPositionAux9 = #{const PA_CHANNEL_POSITION_AUX9}
channelPositionToInt ChannelPositionAux10 = #{const PA_CHANNEL_POSITION_AUX10}
channelPositionToInt ChannelPositionAux11 = #{const PA_CHANNEL_POSITION_AUX11}
channelPositionToInt ChannelPositionAux12 = #{const PA_CHANNEL_POSITION_AUX12}
channelPositionToInt ChannelPositionAux13 = #{const PA_CHANNEL_POSITION_AUX13}
channelPositionToInt ChannelPositionAux14 = #{const PA_CHANNEL_POSITION_AUX14}
channelPositionToInt ChannelPositionAux15 = #{const PA_CHANNEL_POSITION_AUX15}
channelPositionToInt ChannelPositionAux16 = #{const PA_CHANNEL_POSITION_AUX16}
channelPositionToInt ChannelPositionAux17 = #{const PA_CHANNEL_POSITION_AUX17}
channelPositionToInt ChannelPositionAux18 = #{const PA_CHANNEL_POSITION_AUX18}
channelPositionToInt ChannelPositionAux19 = #{const PA_CHANNEL_POSITION_AUX19}
channelPositionToInt ChannelPositionAux20 = #{const PA_CHANNEL_POSITION_AUX20}
channelPositionToInt ChannelPositionAux21 = #{const PA_CHANNEL_POSITION_AUX21}
channelPositionToInt ChannelPositionAux22 = #{const PA_CHANNEL_POSITION_AUX22}
channelPositionToInt ChannelPositionAux23 = #{const PA_CHANNEL_POSITION_AUX23}
channelPositionToInt ChannelPositionAux24 = #{const PA_CHANNEL_POSITION_AUX24}
channelPositionToInt ChannelPositionAux25 = #{const PA_CHANNEL_POSITION_AUX25}
channelPositionToInt ChannelPositionAux26 = #{const PA_CHANNEL_POSITION_AUX26}
channelPositionToInt ChannelPositionAux27 = #{const PA_CHANNEL_POSITION_AUX27}
channelPositionToInt ChannelPositionAux28 = #{const PA_CHANNEL_POSITION_AUX28}
channelPositionToInt ChannelPositionAux29 = #{const PA_CHANNEL_POSITION_AUX29}
channelPositionToInt ChannelPositionAux30 = #{const PA_CHANNEL_POSITION_AUX30}
channelPositionToInt ChannelPositionAux31 = #{const PA_CHANNEL_POSITION_AUX31}
channelPositionToInt ChannelPositionTopCenter = #{const PA_CHANNEL_POSITION_TOP_CENTER}
channelPositionToInt ChannelPositionTopFrontLeft = #{const PA_CHANNEL_POSITION_TOP_FRONT_LEFT}
channelPositionToInt ChannelPositionTopFrontRight = #{const PA_CHANNEL_POSITION_TOP_FRONT_RIGHT}
channelPositionToInt ChannelPositionTopFrontCenter = #{const PA_CHANNEL_POSITION_TOP_FRONT_CENTER}
channelPositionToInt ChannelPositionTopRearLeft = #{const PA_CHANNEL_POSITION_TOP_REAR_LEFT}
channelPositionToInt ChannelPositionTopRearRight = #{const PA_CHANNEL_POSITION_TOP_REAR_RIGHT}
channelPositionToInt ChannelPositionTopRearCenter = #{const PA_CHANNEL_POSITION_TOP_REAR_CENTER}
channelPositionToInt ChannelPositionMax = #{const PA_CHANNEL_POSITION_MAX}

channelPositionFromInt :: CInt -> ChannelPosition
channelPositionFromInt (#{const PA_CHANNEL_POSITION_INVALID}) = ChannelPositionInvalid
channelPositionFromInt (#{const PA_CHANNEL_POSITION_MONO}) = ChannelPositionMono
channelPositionFromInt (#{const PA_CHANNEL_POSITION_FRONT_LEFT}) = ChannelPositionFrontLeft
channelPositionFromInt (#{const PA_CHANNEL_POSITION_FRONT_RIGHT}) = ChannelPositionFrontRight
channelPositionFromInt (#{const PA_CHANNEL_POSITION_FRONT_CENTER}) = ChannelPositionFrontCenter
channelPositionFromInt (#{const PA_CHANNEL_POSITION_LEFT}) = ChannelPositionLeft
channelPositionFromInt (#{const PA_CHANNEL_POSITION_RIGHT}) = ChannelPositionRight
channelPositionFromInt (#{const PA_CHANNEL_POSITION_CENTER}) = ChannelPositionCenter
channelPositionFromInt (#{const PA_CHANNEL_POSITION_REAR_CENTER}) = ChannelPositionRearCenter
channelPositionFromInt (#{const PA_CHANNEL_POSITION_REAR_LEFT}) = ChannelPositionRearLeft
channelPositionFromInt (#{const PA_CHANNEL_POSITION_REAR_RIGHT}) = ChannelPositionRearRight
channelPositionFromInt (#{const PA_CHANNEL_POSITION_LFE}) = ChannelPositionLfe
channelPositionFromInt (#{const PA_CHANNEL_POSITION_SUBWOOFER}) = ChannelPositionSubwoofer
channelPositionFromInt (#{const PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER}) = ChannelPositionFrontLeftOfCenter
channelPositionFromInt (#{const PA_CHANNEL_POSITION_FRONT_RIGHT_OF_CENTER}) = ChannelPositionFrontRightOfCenter
channelPositionFromInt (#{const PA_CHANNEL_POSITION_SIDE_LEFT}) = ChannelPositionSideLeft
channelPositionFromInt (#{const PA_CHANNEL_POSITION_SIDE_RIGHT}) = ChannelPositionSideRight
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX0}) = ChannelPositionAux0
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX1}) = ChannelPositionAux1
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX2}) = ChannelPositionAux2
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX3}) = ChannelPositionAux3
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX4}) = ChannelPositionAux4
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX5}) = ChannelPositionAux5
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX6}) = ChannelPositionAux6
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX7}) = ChannelPositionAux7
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX8}) = ChannelPositionAux8
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX9}) = ChannelPositionAux9
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX10}) = ChannelPositionAux10
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX11}) = ChannelPositionAux11
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX12}) = ChannelPositionAux12
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX13}) = ChannelPositionAux13
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX14}) = ChannelPositionAux14
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX15}) = ChannelPositionAux15
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX16}) = ChannelPositionAux16
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX17}) = ChannelPositionAux17
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX18}) = ChannelPositionAux18
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX19}) = ChannelPositionAux19
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX20}) = ChannelPositionAux20
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX21}) = ChannelPositionAux21
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX22}) = ChannelPositionAux22
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX23}) = ChannelPositionAux23
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX24}) = ChannelPositionAux24
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX25}) = ChannelPositionAux25
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX26}) = ChannelPositionAux26
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX27}) = ChannelPositionAux27
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX28}) = ChannelPositionAux28
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX29}) = ChannelPositionAux29
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX30}) = ChannelPositionAux30
channelPositionFromInt (#{const PA_CHANNEL_POSITION_AUX31}) = ChannelPositionAux31
channelPositionFromInt (#{const PA_CHANNEL_POSITION_TOP_CENTER}) = ChannelPositionTopCenter
channelPositionFromInt (#{const PA_CHANNEL_POSITION_TOP_FRONT_LEFT}) = ChannelPositionTopFrontLeft
channelPositionFromInt (#{const PA_CHANNEL_POSITION_TOP_FRONT_RIGHT}) = ChannelPositionTopFrontRight
channelPositionFromInt (#{const PA_CHANNEL_POSITION_TOP_FRONT_CENTER}) = ChannelPositionTopFrontCenter
channelPositionFromInt (#{const PA_CHANNEL_POSITION_TOP_REAR_LEFT}) = ChannelPositionTopRearLeft
channelPositionFromInt (#{const PA_CHANNEL_POSITION_TOP_REAR_RIGHT}) = ChannelPositionTopRearRight
channelPositionFromInt (#{const PA_CHANNEL_POSITION_TOP_REAR_CENTER}) = ChannelPositionTopRearCenter
channelPositionFromInt (#{const PA_CHANNEL_POSITION_MAX}) = ChannelPositionMax
channelPositionFromInt x = error ("PA unexped value @channelPositionFromInt:" ++ show x)
data ChannelMapDef
    = ChannelMapAiff
    | ChannelMapAlsa
    | ChannelMapAux
    | ChannelMapWaveex
    | ChannelMapOss
    | ChannelMapDefMax
    | ChannelMapDefault
    deriving (Eq, Show)

channelMapDefToInt :: ChannelMapDef -> CInt
channelMapDefToInt ChannelMapAiff = #{const PA_CHANNEL_MAP_AIFF}
channelMapDefToInt ChannelMapAlsa = #{const PA_CHANNEL_MAP_ALSA}
channelMapDefToInt ChannelMapAux = #{const PA_CHANNEL_MAP_AUX}
channelMapDefToInt ChannelMapWaveex = #{const PA_CHANNEL_MAP_WAVEEX}
channelMapDefToInt ChannelMapOss = #{const PA_CHANNEL_MAP_OSS}
channelMapDefToInt ChannelMapDefMax = #{const PA_CHANNEL_MAP_DEF_MAX}
channelMapDefToInt ChannelMapDefault = #{const PA_CHANNEL_MAP_DEFAULT}

channelMapDefFromInt :: CInt -> ChannelMapDef
channelMapDefFromInt (#{const PA_CHANNEL_MAP_AIFF}) = ChannelMapAiff
channelMapDefFromInt (#{const PA_CHANNEL_MAP_ALSA}) = ChannelMapAlsa
channelMapDefFromInt (#{const PA_CHANNEL_MAP_AUX}) = ChannelMapAux
channelMapDefFromInt (#{const PA_CHANNEL_MAP_WAVEEX}) = ChannelMapWaveex
channelMapDefFromInt (#{const PA_CHANNEL_MAP_OSS}) = ChannelMapOss
channelMapDefFromInt (#{const PA_CHANNEL_MAP_DEF_MAX}) = ChannelMapDefMax
channelMapDefFromInt (#{const PA_CHANNEL_MAP_DEFAULT}) = ChannelMapDefault
channelMapDefFromInt x = error ("PA unexped value @channelMapDefFromInt:" ++ show x)
data SampleFormat
    = SampleU8
    | SampleAlaw
    | SampleUlaw
    | SampleS16le
    | SampleS16be
    | SampleFloat32le
    | SampleFloat32be
    | SampleS32le
    | SampleS32be
    | SampleS24le
    | SampleS24be
    | SampleS2432le
    | SampleS2432be
    | SampleMax
    | SampleInvalid
    deriving (Eq, Show)

sampleFormatToInt :: SampleFormat -> CInt
sampleFormatToInt SampleU8 = #{const PA_SAMPLE_U8}
sampleFormatToInt SampleAlaw = #{const PA_SAMPLE_ALAW}
sampleFormatToInt SampleUlaw = #{const PA_SAMPLE_ULAW}
sampleFormatToInt SampleS16le = #{const PA_SAMPLE_S16LE}
sampleFormatToInt SampleS16be = #{const PA_SAMPLE_S16BE}
sampleFormatToInt SampleFloat32le = #{const PA_SAMPLE_FLOAT32LE}
sampleFormatToInt SampleFloat32be = #{const PA_SAMPLE_FLOAT32BE}
sampleFormatToInt SampleS32le = #{const PA_SAMPLE_S32LE}
sampleFormatToInt SampleS32be = #{const PA_SAMPLE_S32BE}
sampleFormatToInt SampleS24le = #{const PA_SAMPLE_S24LE}
sampleFormatToInt SampleS24be = #{const PA_SAMPLE_S24BE}
sampleFormatToInt SampleS2432le = #{const PA_SAMPLE_S24_32LE}
sampleFormatToInt SampleS2432be = #{const PA_SAMPLE_S24_32BE}
sampleFormatToInt SampleMax = #{const PA_SAMPLE_MAX}
sampleFormatToInt SampleInvalid = #{const PA_SAMPLE_INVALID}

sampleFormatFromInt :: CInt -> SampleFormat
sampleFormatFromInt (#{const PA_SAMPLE_U8}) = SampleU8
sampleFormatFromInt (#{const PA_SAMPLE_ALAW}) = SampleAlaw
sampleFormatFromInt (#{const PA_SAMPLE_ULAW}) = SampleUlaw
sampleFormatFromInt (#{const PA_SAMPLE_S16LE}) = SampleS16le
sampleFormatFromInt (#{const PA_SAMPLE_S16BE}) = SampleS16be
sampleFormatFromInt (#{const PA_SAMPLE_FLOAT32LE}) = SampleFloat32le
sampleFormatFromInt (#{const PA_SAMPLE_FLOAT32BE}) = SampleFloat32be
sampleFormatFromInt (#{const PA_SAMPLE_S32LE}) = SampleS32le
sampleFormatFromInt (#{const PA_SAMPLE_S32BE}) = SampleS32be
sampleFormatFromInt (#{const PA_SAMPLE_S24LE}) = SampleS24le
sampleFormatFromInt (#{const PA_SAMPLE_S24BE}) = SampleS24be
sampleFormatFromInt (#{const PA_SAMPLE_S24_32LE}) = SampleS2432le
sampleFormatFromInt (#{const PA_SAMPLE_S24_32BE}) = SampleS2432be
sampleFormatFromInt (#{const PA_SAMPLE_MAX}) = SampleMax
sampleFormatFromInt (#{const PA_SAMPLE_INVALID}) = SampleInvalid
sampleFormatFromInt x = error ("PA unexped value @sampleFormatFromInt:" ++ show x)
data ContextFlags
    = ContextNoflags
    | ContextNoautospawn
    | ContextNofail
    deriving (Eq, Show)

contextFlagsToInt :: ContextFlags -> CInt
contextFlagsToInt ContextNoflags = #{const PA_CONTEXT_NOFLAGS}
contextFlagsToInt ContextNoautospawn = #{const PA_CONTEXT_NOAUTOSPAWN}
contextFlagsToInt ContextNofail = #{const PA_CONTEXT_NOFAIL}

contextFlagssToInt :: [ContextFlags] -> CInt
contextFlagssToInt = foldFlag contextFlagsToInt

contextFlagssFromInt :: CInt -> [ContextFlags]
contextFlagssFromInt i =
    let
        t0 = if (i .&. #{const PA_CONTEXT_NOFLAGS} /= 0) then (ContextNoflags:) else id
        t1 = if (i .&. #{const PA_CONTEXT_NOAUTOSPAWN} /= 0) then (ContextNoautospawn:) else id
        t2 = if (i .&. #{const PA_CONTEXT_NOFAIL} /= 0) then (ContextNofail:) else id
    in t0 . t1 . t2 . id $ []
data SubscriptionMask
    = SubscriptionMaskNull
    | SubscriptionMaskSink
    | SubscriptionMaskSource
    | SubscriptionMaskSinkInput
    | SubscriptionMaskSourceOutput
    | SubscriptionMaskModule
    | SubscriptionMaskClient
    | SubscriptionMaskSampleCache
    | SubscriptionMaskServer
    | SubscriptionMaskAutoload
    | SubscriptionMaskCard
    | SubscriptionMaskAll
    deriving (Eq, Show)

subscriptionMaskToInt :: SubscriptionMask -> CInt
subscriptionMaskToInt SubscriptionMaskNull = #{const PA_SUBSCRIPTION_MASK_NULL}
subscriptionMaskToInt SubscriptionMaskSink = #{const PA_SUBSCRIPTION_MASK_SINK}
subscriptionMaskToInt SubscriptionMaskSource = #{const PA_SUBSCRIPTION_MASK_SOURCE}
subscriptionMaskToInt SubscriptionMaskSinkInput = #{const PA_SUBSCRIPTION_MASK_SINK_INPUT}
subscriptionMaskToInt SubscriptionMaskSourceOutput = #{const PA_SUBSCRIPTION_MASK_SOURCE_OUTPUT}
subscriptionMaskToInt SubscriptionMaskModule = #{const PA_SUBSCRIPTION_MASK_MODULE}
subscriptionMaskToInt SubscriptionMaskClient = #{const PA_SUBSCRIPTION_MASK_CLIENT}
subscriptionMaskToInt SubscriptionMaskSampleCache = #{const PA_SUBSCRIPTION_MASK_SAMPLE_CACHE}
subscriptionMaskToInt SubscriptionMaskServer = #{const PA_SUBSCRIPTION_MASK_SERVER}
subscriptionMaskToInt SubscriptionMaskAutoload = #{const PA_SUBSCRIPTION_MASK_AUTOLOAD}
subscriptionMaskToInt SubscriptionMaskCard = #{const PA_SUBSCRIPTION_MASK_CARD}
subscriptionMaskToInt SubscriptionMaskAll = #{const PA_SUBSCRIPTION_MASK_ALL}

subscriptionMasksToInt :: [SubscriptionMask] -> CInt
subscriptionMasksToInt = foldFlag subscriptionMaskToInt

subscriptionMasksFromInt :: CInt -> [SubscriptionMask]
subscriptionMasksFromInt i =
    let
        t0 = if (i .&. #{const PA_SUBSCRIPTION_MASK_NULL} /= 0) then (SubscriptionMaskNull:) else id
        t1 = if (i .&. #{const PA_SUBSCRIPTION_MASK_SINK} /= 0) then (SubscriptionMaskSink:) else id
        t2 = if (i .&. #{const PA_SUBSCRIPTION_MASK_SOURCE} /= 0) then (SubscriptionMaskSource:) else id
        t3 = if (i .&. #{const PA_SUBSCRIPTION_MASK_SINK_INPUT} /= 0) then (SubscriptionMaskSinkInput:) else id
        t4 = if (i .&. #{const PA_SUBSCRIPTION_MASK_SOURCE_OUTPUT} /= 0) then (SubscriptionMaskSourceOutput:) else id
        t5 = if (i .&. #{const PA_SUBSCRIPTION_MASK_MODULE} /= 0) then (SubscriptionMaskModule:) else id
        t6 = if (i .&. #{const PA_SUBSCRIPTION_MASK_CLIENT} /= 0) then (SubscriptionMaskClient:) else id
        t7 = if (i .&. #{const PA_SUBSCRIPTION_MASK_SAMPLE_CACHE} /= 0) then (SubscriptionMaskSampleCache:) else id
        t8 = if (i .&. #{const PA_SUBSCRIPTION_MASK_SERVER} /= 0) then (SubscriptionMaskServer:) else id
        t9 = if (i .&. #{const PA_SUBSCRIPTION_MASK_AUTOLOAD} /= 0) then (SubscriptionMaskAutoload:) else id
        t10 = if (i .&. #{const PA_SUBSCRIPTION_MASK_CARD} /= 0) then (SubscriptionMaskCard:) else id
        t11 = if (i .&. #{const PA_SUBSCRIPTION_MASK_ALL} /= 0) then (SubscriptionMaskAll:) else id
    in t0 . t1 . t2 . t3 . t4 . t5 . t6 . t7 . t8 . t9 . t10 . t11 . id $ []
data SinkFlags
    = SinkNoflags
    | SinkHwVolumeCtrl
    | SinkLatency
    | SinkHardware
    | SinkNetwork
    | SinkHwMuteCtrl
    | SinkDecibelVolume
    | SinkFlatVolume
    | SinkDynamicLatency
    | SinkSetFormats
    deriving (Eq, Show)

sinkFlagsToInt :: SinkFlags -> CInt
sinkFlagsToInt SinkNoflags = #{const PA_SINK_NOFLAGS}
sinkFlagsToInt SinkHwVolumeCtrl = #{const PA_SINK_HW_VOLUME_CTRL}
sinkFlagsToInt SinkLatency = #{const PA_SINK_LATENCY}
sinkFlagsToInt SinkHardware = #{const PA_SINK_HARDWARE}
sinkFlagsToInt SinkNetwork = #{const PA_SINK_NETWORK}
sinkFlagsToInt SinkHwMuteCtrl = #{const PA_SINK_HW_MUTE_CTRL}
sinkFlagsToInt SinkDecibelVolume = #{const PA_SINK_DECIBEL_VOLUME}
sinkFlagsToInt SinkFlatVolume = #{const PA_SINK_FLAT_VOLUME}
sinkFlagsToInt SinkDynamicLatency = #{const PA_SINK_DYNAMIC_LATENCY}
sinkFlagsToInt SinkSetFormats = #{const PA_SINK_SET_FORMATS}

sinkFlagssToInt :: [SinkFlags] -> CInt
sinkFlagssToInt = foldFlag sinkFlagsToInt

sinkFlagssFromInt :: CInt -> [SinkFlags]
sinkFlagssFromInt i =
    let
        t0 = if (i .&. #{const PA_SINK_NOFLAGS} /= 0) then (SinkNoflags:) else id
        t1 = if (i .&. #{const PA_SINK_HW_VOLUME_CTRL} /= 0) then (SinkHwVolumeCtrl:) else id
        t2 = if (i .&. #{const PA_SINK_LATENCY} /= 0) then (SinkLatency:) else id
        t3 = if (i .&. #{const PA_SINK_HARDWARE} /= 0) then (SinkHardware:) else id
        t4 = if (i .&. #{const PA_SINK_NETWORK} /= 0) then (SinkNetwork:) else id
        t5 = if (i .&. #{const PA_SINK_HW_MUTE_CTRL} /= 0) then (SinkHwMuteCtrl:) else id
        t6 = if (i .&. #{const PA_SINK_DECIBEL_VOLUME} /= 0) then (SinkDecibelVolume:) else id
        t7 = if (i .&. #{const PA_SINK_FLAT_VOLUME} /= 0) then (SinkFlatVolume:) else id
        t8 = if (i .&. #{const PA_SINK_DYNAMIC_LATENCY} /= 0) then (SinkDynamicLatency:) else id
        t9 = if (i .&. #{const PA_SINK_SET_FORMATS} /= 0) then (SinkSetFormats:) else id
    in t0 . t1 . t2 . t3 . t4 . t5 . t6 . t7 . t8 . t9 . id $ []
data SourceFlags
    = SourceNoflags
    | SourceHwVolumeCtrl
    | SourceLatency
    | SourceHardware
    | SourceNetwork
    | SourceHwMuteCtrl
    | SourceDecibelVolume
    | SourceDynamicLatency
    | SourceFlatVolume
    deriving (Eq, Show)

sourceFlagsToInt :: SourceFlags -> CInt
sourceFlagsToInt SourceNoflags = #{const PA_SOURCE_NOFLAGS}
sourceFlagsToInt SourceHwVolumeCtrl = #{const PA_SOURCE_HW_VOLUME_CTRL}
sourceFlagsToInt SourceLatency = #{const PA_SOURCE_LATENCY}
sourceFlagsToInt SourceHardware = #{const PA_SOURCE_HARDWARE}
sourceFlagsToInt SourceNetwork = #{const PA_SOURCE_NETWORK}
sourceFlagsToInt SourceHwMuteCtrl = #{const PA_SOURCE_HW_MUTE_CTRL}
sourceFlagsToInt SourceDecibelVolume = #{const PA_SOURCE_DECIBEL_VOLUME}
sourceFlagsToInt SourceDynamicLatency = #{const PA_SOURCE_DYNAMIC_LATENCY}
sourceFlagsToInt SourceFlatVolume = #{const PA_SOURCE_FLAT_VOLUME}

sourceFlagssToInt :: [SourceFlags] -> CInt
sourceFlagssToInt = foldFlag sourceFlagsToInt

sourceFlagssFromInt :: CInt -> [SourceFlags]
sourceFlagssFromInt i =
    let
        t0 = if (i .&. #{const PA_SOURCE_NOFLAGS} /= 0) then (SourceNoflags:) else id
        t1 = if (i .&. #{const PA_SOURCE_HW_VOLUME_CTRL} /= 0) then (SourceHwVolumeCtrl:) else id
        t2 = if (i .&. #{const PA_SOURCE_LATENCY} /= 0) then (SourceLatency:) else id
        t3 = if (i .&. #{const PA_SOURCE_HARDWARE} /= 0) then (SourceHardware:) else id
        t4 = if (i .&. #{const PA_SOURCE_NETWORK} /= 0) then (SourceNetwork:) else id
        t5 = if (i .&. #{const PA_SOURCE_HW_MUTE_CTRL} /= 0) then (SourceHwMuteCtrl:) else id
        t6 = if (i .&. #{const PA_SOURCE_DECIBEL_VOLUME} /= 0) then (SourceDecibelVolume:) else id
        t7 = if (i .&. #{const PA_SOURCE_DYNAMIC_LATENCY} /= 0) then (SourceDynamicLatency:) else id
        t8 = if (i .&. #{const PA_SOURCE_FLAT_VOLUME} /= 0) then (SourceFlatVolume:) else id
    in t0 . t1 . t2 . t3 . t4 . t5 . t6 . t7 . t8 . id $ []

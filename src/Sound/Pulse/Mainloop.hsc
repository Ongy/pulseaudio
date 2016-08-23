{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
module Sound.Pulse.Mainloop
    ( PAMainloop(..)
    , PAIOEventFlags(..)
    , PAMainloopApi
    , getMainloopApi

    )
where


#include <pulse/mainloop-api.h>

import Foreign.Marshal.Utils (with)
import Foreign.Storable
import Foreign.StablePtr
import Foreign.Ptr
import Foreign.C.Types
import System.Posix.Types (Fd(..))

import Data.Bits ((.&.), (.|.))

import Control.Concurrent.MVar

import Data.Time
import Data.Time.Internal


data PAIOEventFlags
    = PAIOEventNull
    | PAIOEventInput
    | PAIOEventOutput
    | PAIOEventHangup
    | PAIOEventError
    deriving (Eq, Show, Ord)

ioEventToCInt :: PAIOEventFlags -> CInt
ioEventToCInt PAIOEventNull   = #{const PA_IO_EVENT_NULL}
ioEventToCInt PAIOEventInput  = #{const PA_IO_EVENT_INPUT}
ioEventToCInt PAIOEventOutput = #{const PA_IO_EVENT_OUTPUT}
ioEventToCInt PAIOEventHangup = #{const PA_IO_EVENT_HANGUP}
ioEventToCInt PAIOEventError  = #{const PA_IO_EVENT_ERROR}

ioEventsToCInt :: [PAIOEventFlags] -> CInt
ioEventsToCInt xs = foldr (\x y -> ioEventToCInt x .|. y) 0 xs

ioEventsFromCInt :: CInt -> [PAIOEventFlags]
ioEventsFromCInt val =
    let enull  = potAdd PAIOEventNull   []
        input  = potAdd PAIOEventInput  enull
        output = potAdd PAIOEventOutput input
        hangup = potAdd PAIOEventHangup output
        err    = potAdd PAIOEventError  hangup
    in err
    where potAdd :: PAIOEventFlags -> ([PAIOEventFlags] -> [PAIOEventFlags])
          potAdd con = if (val .&. ioEventToCInt con) /= 0 then (con:) else id

class PAMainloop a where
    -- |Pulseaudio IO event
    data PAIOEvent a :: *
    -- |Pulseaudio Time event
    data PATimeEvent a :: *
    -- |Pulseaudio Defer event
    data PADeferEvent a :: *

    ioNew    :: a -> Fd -> [PAIOEventFlags] -> ([PAIOEventFlags] -> IO ()) -> IO (PAIOEvent a)
    ioEnable :: (PAIOEvent a) -> [PAIOEventFlags] -> IO ()
    ioFree   :: (PAIOEvent a) -> IO ()
    ioSetDestroy :: (PAIOEvent a) -> IO () -> IO ()

    timeNew :: a -> PATime -> (PATime -> IO ()) -> IO (PATimeEvent a)
    timeRestart :: PATimeEvent a -> PATime -> IO ()
    timeFree :: PATimeEvent a -> IO ()
    timeSetDestroy :: PATimeEvent a -> IO () -> IO ()

    deferNew :: a -> IO () -> IO (PADeferEvent a)
    deferEnable :: PADeferEvent a -> Bool -> IO ()
    deferFree :: PADeferEvent a -> IO ()
    deferSetDestroy :: PADeferEvent a -> IO () -> IO ()

    quitLoop :: a -> Int -> IO ()

data Userdata

data PAIOEventW a = PAIOEventW
    {- ioRealHandle  :: -} (PAIOEvent a)
    {- ioWrappedData :: -} (Ptr Userdata)
    {- ioWrappedApi  :: -} (Ptr (PAMainloopApi a))

ioRealHandle :: PAIOEventW a -> PAIOEvent a
ioRealHandle (PAIOEventW x _ _) = x

data PATimeEventW a = PATimeEventW
    {- timeRealHandle  :: -} (PATimeEvent a)
    {- timeWrappedData :: -} (Ptr Userdata)
    {- timeWrappedApi  :: -} (Ptr (PAMainloopApi a))

timeRealHandle :: PATimeEventW a -> PATimeEvent a
timeRealHandle (PATimeEventW x _ _) = x

data PADeferEventW a = PADeferEventW
    {- deferRealHandle  :: -} (PADeferEvent a)
    {- deferWrappedData :: -} (Ptr Userdata)
    {- deferWrappedApi  :: -} (Ptr (PAMainloopApi a))

deferRealHandle :: PADeferEventW a -> PADeferEvent a
deferRealHandle (PADeferEventW x _ _) = x

{- IO Events -}
type IOEvCB a = Ptr (PAMainloopApi a) -> StablePtr (PAIOEventW a) -> CInt -> CInt -> Ptr Userdata -> IO ()
foreign import ccall "dynamic" mkIOEvCB :: FunPtr (IOEvCB a) -> (IOEvCB a)

type IONew a = Ptr (PAMainloopApi a) -> CInt -> CInt -> FunPtr (IOEvCB a) -> Ptr Userdata -> IO (StablePtr (PAIOEventW a))
foreign import ccall "wrapper" mkIONew :: IONew a -> IO (FunPtr (IONew a))

type IOEnable a = StablePtr (PAIOEventW a) -> CInt -> IO ()
foreign import ccall "wrapper" mkIOEnable :: IOEnable a -> IO (FunPtr (IOEnable a))

type IOFree a = StablePtr (PAIOEventW a) -> IO ()
foreign import ccall "wrapper" mkIOFree :: IOFree a -> IO (FunPtr (IOFree a))

type IODestroy a = Ptr (PAMainloopApi a) -> StablePtr (PAIOEventW a) -> Ptr Userdata -> IO ()
foreign import ccall "dynamic" mkIODestroy :: FunPtr (IODestroy a) -> (IODestroy a)

type IOSetDestroy a = StablePtr (PAIOEventW a) -> FunPtr (IODestroy a) -> IO ()
foreign import ccall "wrapper" mkIOSetDestroy :: IOSetDestroy a -> IO (FunPtr (IOSetDestroy a))

{- Time Events -}

type TimeEvCB a = Ptr (PAMainloopApi a) -> StablePtr (PATimeEventW a) -> Ptr PAITime -> Ptr Userdata -> IO ()
foreign import ccall "dynamic" mkTimeEvCB :: FunPtr (TimeEvCB a) -> (TimeEvCB a)

type TimeNew a = Ptr (PAMainloopApi a) -> Ptr PAITime -> FunPtr (TimeEvCB a) -> Ptr Userdata -> IO (StablePtr (PATimeEventW a))
foreign import ccall "wrapper" mkTimeNew :: TimeNew a -> IO (FunPtr (TimeNew a))

type TimeRestart a = StablePtr (PATimeEventW a) -> Ptr PAITime -> IO ()
foreign import ccall "wrapper" mkTimeRestart :: TimeRestart a -> IO (FunPtr (TimeRestart a))

type TimeFree a = StablePtr (PATimeEventW a) -> IO ()
foreign import ccall "wrapper" mkTimeFree :: TimeFree a -> IO (FunPtr (TimeFree a))

type TimeDestroy a = Ptr (PAMainloopApi a) -> StablePtr (PATimeEventW a) -> Ptr Userdata -> IO ()
foreign import ccall "dynamic" mkTimeDestroy :: FunPtr (TimeDestroy a) -> (TimeDestroy a)

type TimeSetDestroy a = StablePtr (PATimeEventW a) -> FunPtr (TimeDestroy a) -> IO ()
foreign import ccall "wrapper" mkTimeSetDestroy :: TimeSetDestroy a -> IO (FunPtr (TimeSetDestroy a))

{- Defer Events -}

type DeferEvCB a = Ptr (PAMainloopApi a) -> StablePtr (PADeferEventW a) -> Ptr Userdata -> IO ()
foreign import ccall "dynamic" mkDeferEvCB :: FunPtr (DeferEvCB a) -> DeferEvCB a

type DeferNew a = Ptr (PAMainloopApi a) -> FunPtr (DeferEvCB a) -> Ptr Userdata -> IO (StablePtr (PADeferEventW a))
foreign import ccall "wrapper" mkDeferNew :: DeferNew a -> IO (FunPtr (DeferNew a))

type DeferEnable a = StablePtr (PADeferEventW a) -> CInt -> IO ()
foreign import ccall "wrapper" mkDeferEnable :: DeferEnable a -> IO (FunPtr (DeferEnable a))

type DeferFree a = StablePtr (PADeferEventW a) -> IO ()
foreign import ccall "wrapper" mkDeferFree :: DeferFree a -> IO (FunPtr (DeferFree a))

type DeferDestroy a = Ptr (PAMainloopApi a) -> StablePtr (PADeferEventW a) -> Ptr Userdata -> IO ()
foreign import ccall "dynamic" mkDeferDestroy :: FunPtr (DeferDestroy a) -> (DeferDestroy a)

type DeferSetDestroy a = StablePtr (PADeferEventW a) -> FunPtr (DeferDestroy a) -> IO ()
foreign import ccall "wrapper" mkDeferSetDestroy :: DeferSetDestroy a -> IO (FunPtr (DeferSetDestroy a))

type Quit a = Ptr (PAMainloopApi a) -> CInt -> IO ()
foreign import ccall "wrapper" mkQuit :: Quit a -> IO (FunPtr (Quit a))

data PAMainloopApi a = PAMainloopApi
    { userdata          :: StablePtr a

    , io_new            :: FunPtr (IONew a)
    , io_enable         :: FunPtr (IOEnable a)
    , io_free           :: FunPtr (IOFree a)
    , io_set_destroy    :: FunPtr (IOSetDestroy a)

    , time_new          :: FunPtr (TimeNew a)
    , time_restart      :: FunPtr (TimeRestart a)
    , time_free         :: FunPtr (TimeFree a)
    , time_set_destroy  :: FunPtr (TimeSetDestroy a)

    , defer_new         :: FunPtr (DeferNew a)
    , defer_enable      :: FunPtr (DeferEnable a)
    , defer_free        :: FunPtr (DeferFree a)
    , defer_set_destroy :: FunPtr (DeferSetDestroy a)

    , quit              :: FunPtr (Quit a)
    }

instance Storable (PAMainloopApi a) where
    sizeOf _ = #{size pa_mainloop_api}
    alignment _ = alignment (undefined :: Word)
    peek p = PAMainloopApi
        <$> #{peek pa_mainloop_api, userdata} p
        <*> #{peek pa_mainloop_api, io_new} p
        <*> #{peek pa_mainloop_api, io_enable} p
        <*> #{peek pa_mainloop_api, io_free} p
        <*> #{peek pa_mainloop_api, io_set_destroy} p
        <*> #{peek pa_mainloop_api, time_new} p
        <*> #{peek pa_mainloop_api, time_restart} p
        <*> #{peek pa_mainloop_api, time_free} p
        <*> #{peek pa_mainloop_api, time_set_destroy} p
        <*> #{peek pa_mainloop_api, defer_new} p
        <*> #{peek pa_mainloop_api, defer_enable} p
        <*> #{peek pa_mainloop_api, defer_free} p
        <*> #{peek pa_mainloop_api, defer_set_destroy} p
        <*> #{peek pa_mainloop_api, quit} p
    poke p (PAMainloopApi {..}) = do
        #{poke pa_mainloop_api, userdata}          p userdata
        #{poke pa_mainloop_api, io_new}            p io_new
        #{poke pa_mainloop_api, io_enable}         p io_enable
        #{poke pa_mainloop_api, io_free}           p io_free
        #{poke pa_mainloop_api, io_set_destroy}    p io_set_destroy
        #{poke pa_mainloop_api, time_new}          p time_new
        #{poke pa_mainloop_api, time_restart}      p time_restart
        #{poke pa_mainloop_api, time_free}         p time_free
        #{poke pa_mainloop_api, time_set_destroy}  p time_set_destroy
        #{poke pa_mainloop_api, defer_new}         p defer_new
        #{poke pa_mainloop_api, defer_enable}      p defer_enable
        #{poke pa_mainloop_api, defer_free}        p defer_free
        #{poke pa_mainloop_api, defer_set_destroy} p defer_set_destroy
        #{poke pa_mainloop_api, quit}              p quit
-- #{let poke_api field = "#{poke pa_mainloop_api, " field "} p $ " field " api"}
-- #{poke_api "userdata"}

getMainloopImpl :: Ptr (PAMainloopApi a) -> IO a
getMainloopImpl p =
    deRefStablePtr =<< (peek . #{ptr pa_mainloop_api, userdata} $ p)

getMainloopApi :: PAMainloop a => a -> IO (PAMainloopApi a)
getMainloopApi api = do
    io_enable <- mkIOEnable $ \ptr flags -> do
        evt <- deRefStablePtr ptr
        ioEnable (ioRealHandle evt) (ioEventsFromCInt flags)
    io_new <- mkIONew $ \a fd flags fptr usr -> do
        -- This is stupid :(
        let fun = mkIOEvCB fptr
        impl <- getMainloopImpl a
        mvar <- newEmptyMVar
        ret <- ioNew impl (Fd fd) (ioEventsFromCInt flags) $ \iflags -> do
            evtp <- readMVar mvar
            fun a evtp fd (ioEventsToCInt iflags) usr
        rpt <- newStablePtr $ PAIOEventW ret usr a
        putMVar mvar rpt
        return rpt
    io_free <- mkIOFree ((=<<) (ioFree . ioRealHandle) . deRefStablePtr)
    io_set_destroy <- mkIOSetDestroy $ \ptr fptr -> do
        let fun = mkIODestroy fptr
        (PAIOEventW evt usr a) <- deRefStablePtr ptr
        ioSetDestroy evt $ fun a ptr usr

    time_new <- mkTimeNew $ \a timep fptr usr -> do
        let fun = mkTimeEvCB fptr
        impl <- getMainloopImpl a
        mvar <- newEmptyMVar
        time <- peek timep
        ret <- timeNew impl (fromPAI time) $ \itime -> do
            evt <- readMVar mvar
            with (toPAI itime) $ flip (fun a evt) usr
        rpt <- newStablePtr $ PATimeEventW ret usr a
        putMVar mvar rpt
        return rpt
    time_restart <- mkTimeRestart $ \ptr timep -> do
        time <- peek timep
        flip timeRestart (fromPAI time) . timeRealHandle =<< deRefStablePtr ptr
    time_free <- mkTimeFree ((=<<) (timeFree . timeRealHandle) . deRefStablePtr)
    time_set_destroy <- mkTimeSetDestroy $ \ptr fptr -> do
        let fun = mkTimeDestroy fptr
        (PATimeEventW evt usr a) <- deRefStablePtr ptr
        timeSetDestroy evt $ fun a ptr usr

    defer_new <- mkDeferNew $ \a fptr usr -> do
        let fun = mkDeferEvCB fptr
        impl <- getMainloopImpl a
        mvar <- newEmptyMVar
        ret <- deferNew impl $ (flip (fun a) usr =<< readMVar mvar)
        rpt <- newStablePtr $ PADeferEventW ret usr a
        putMVar mvar rpt
        return rpt
    defer_enable <- mkDeferEnable $ \ptr switch -> do
        flip deferEnable (switch /= 0) . deferRealHandle =<< deRefStablePtr ptr
    defer_free <- mkDeferFree ((=<<) (deferFree . deferRealHandle) . deRefStablePtr)
    defer_set_destroy <- mkDeferSetDestroy $ \ptr fptr -> do
        let fun = mkDeferDestroy fptr
        (PADeferEventW evt usr a) <- deRefStablePtr ptr
        deferSetDestroy evt $ fun a ptr usr

    quit <- mkQuit $ \ptr val -> do
        impl <- getMainloopImpl ptr
        quitLoop impl (fromIntegral val)
    userdata <- newStablePtr api
    return $ PAMainloopApi {..}

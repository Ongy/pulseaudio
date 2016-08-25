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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-|
Module      : Sound.Pulse.Mainloop.Simple
Description : provides a simple (buggy?) implementation of the pulse mainloop.
Maintianer  : ongy
Stability   : experimental

This implementation lacks support for anything but IOInput and IOOutput!.

The main appeal of this is, that it runs in the Haskell RTS system and does not
block the RTS in any way.
If the 'doLoop' function is called in an 'forkIO', all functions will "just work",
and may be wrapped into a synchronous api wrapper.

If the callback style application flow should be used, this will dispatch all
callbacks in the same (Haskell)-thread.

Most function *should* be thread safe (adding multiple events etc.) but there are
no checks done for incompatible things happening (modifying and removing the same event).
-}
module Sound.Pulse.Mainloop.Simple
    ( MainloopImpl
    , getMainloopImpl
    , doIteration
    , doLoop
    )
where

import Data.Word (Word)
import Control.Applicative ((<$>), (<*>))
import System.Timeout (timeout)
import Data.Maybe (listToMaybe, fromJust, isJust)
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (void, join, when, filterM)
import Data.IORef
import Data.List (insertBy, delete, deleteBy)
import Data.Ord (comparing)
import System.Posix.IO (createPipe, fdWrite, fdRead)
import System.Posix.Types (Fd(..))

import Sound.Pulse.Mainloop
import Data.Time

-- Has to keep state internally because of StablePtrs being passed around
-- |The 'PAIOEvent' type for this implementation
data IOEvent = IOEvent
    { ioCallback :: [PAIOEventFlags] -> IO ()
    , ioFd       :: Fd
    , ioImpl     :: MainloopImpl
    , ioID       :: Word
    , ioEvents   :: IORef ([PAIOEventFlags])
    , ioDestroy  :: IORef (IO ())
    }

instance Eq IOEvent where
    x == y = ioID x == ioID y

-- |The 'PATimeEvent' type for this implementation
data TimeEvent = TimeEvent
    { timeCallback :: PATime -> IO ()
    , timeImpl     :: MainloopImpl
    , timeID       :: Word
    , timeDeadline :: IORef(PATime) -- Can be updated -.-
    , timeDestroy  :: IORef(IO ())
    }

instance Eq TimeEvent where
    x == y = timeID x == timeID y

-- |The 'PADeferEvent' type for this implementation
data DeferEvent = DeferEvent
    { deferCallback :: IO ()
    , deferImpl     :: MainloopImpl
    , deferID       :: Word
    , deferEnabled  :: IORef (Bool)
    , deferDestroy  :: IORef (IO ())
    }

instance Eq DeferEvent where
    x == y = deferID x == deferID y

-- |Implementation type, keeping state about existing events.
data MainloopImpl = MainloopImpl
    -- Lists for events
    { implIOEvents    :: IORef [IOEvent]
    , implTimeEvents  :: IORef [(PATime, TimeEvent)] -- ^The first entry will be the first one to expire (or has already expired)
    , implTimeDisable :: IORef [TimeEvent] -- ^The first entry will be the first one to expire (or has already expired)
    , implDeferEvents :: IORef [DeferEvent]

    -- Counters for event ID
    , implIOCount     :: IORef Word
    , implTimeCount   :: IORef Word
    , implDeferCount  :: IORef Word

    -- If set to Just x, return x
    , implRunning     :: IORef (Maybe Int)
    , implPipe        :: (Fd, Fd)
    }

-- We already checked if we want to read when we are here
waitReadEvent :: IOEvent -> IO (STM (PAIOEventFlags, IOEvent))
waitReadEvent evt = do
    (wait, _) <- threadWaitReadSTM $ ioFd evt
    return $ do
        wait
        return (PAIOEventInput, evt)

-- We already checked if we want to write when we are here
waitWriteEvent :: IOEvent -> IO (STM (PAIOEventFlags, IOEvent))
waitWriteEvent evt = do
    (wait, _) <- threadWaitWriteSTM $ ioFd evt
    return $ do
        wait
        return (PAIOEventOutput, evt)

-- |Split up the IOEvents into different event types.
splitEvents :: [IOEvent] -> IO ([IOEvent], [IOEvent])
splitEvents [] = return ([], [])
splitEvents (x:xs) = do
    (tr, tw) <- splitEvents xs
    events <- readIORef $ ioEvents x
    let cr = if PAIOEventInput `elem` events then (x:) else id
    let cw = if PAIOEventOutput `elem` events then (x:) else id
    when (PAIOEventHangup `elem` events) (fail "PASimple does not support Hangup")
    when (PAIOEventError `elem` events) (fail "PASimple does not support Error")
    return (cr tr, cw tw)

-- |Create the STM event we want to wait on, to get the next event possible.
waitEvents :: MainloopImpl -> IO (STM (PAIOEventFlags, IOEvent))
waitEvents impl = do
    (readEvt, writeEvt) <- splitEvents =<< readIORef (implIOEvents impl)
    readEvts  <- mapM waitReadEvent readEvt
    writeEvts <- mapM waitWriteEvent writeEvt
    let readSTM = foldr (<|>) retry readEvts
    let writeSTM = foldr (<|>) retry writeEvts
    return (readSTM <|> writeSTM)
    --            if null readEvts
    --        then retry
    --        else foldr1 (<|>) readEvts
    --            if null writeEvts
    --        then retry
    --        else foldr1 (<|>) writeEvts
    -- We prefere reading over writing with this!

-- |Do one iteration of waiting for and dispatching events.
doRun :: MainloopImpl -> IO ()
doRun impl = do
    (pipeWait, _) <- threadWaitReadSTM . fst . implPipe $ impl
    wait <- waitEvents impl
    nextT <- fmap snd . listToMaybe <$> readIORef (implTimeEvents impl)
    -- let nextT = Nothing
    capp <- case nextT of
        Nothing -> return (fmap Just)
        Just x -> do
            now <- getTime
            evt <- readIORef $ timeDeadline x
            -- putStrLn ("Waiting for: " ++ show (timeToUS (getDiff evt now)))
            return . timeout $ if now > evt
               then 0
               else fromIntegral (timeToUS (getDiff evt now))
    ret <- capp (atomically ((Right <$> wait) <|> (Left <$> pipeWait)))
    case ret of
      -- if it is nothing we know that we have a timeout
        Nothing -> do
            let evt = fromJust nextT
            time <- readIORef $ timeDeadline evt
            atomModifyIORef (implTimeEvents impl) tail
            atomModifyIORef (implTimeDisable impl) (evt:)
            timeCallback evt time
        -- Just Right -> We got an IO Event
        Just (Right (flag, evt)) -> do
            ioCallback evt [flag]
        -- Just Left -> We got woken up (read on pipe)
        -- We have to throw away what we wrote to wake up, so read
        Just (Left _) -> do
            _ <- fdRead (fst . implPipe $ impl) 512
            return ()

-- |Do one iteration of events. This may be dispatchin defered events,
-- a timer, an io event, or simply a wakeup.
-- Event callback will be called in the same thread as this!
doIteration :: MainloopImpl -> IO ()
doIteration impl = do
    defers <- readIORef $ implDeferEvents impl
    actives <- filterM (readIORef . deferEnabled) defers
    if null actives
       then doRun impl
       else do
           mapM_ deferCallback actives

-- |Loop in the pulse main loop until eternety
doLoop :: MainloopImpl -> IO ()
doLoop impl = do
    doIteration impl
    cont <- readIORef $ implRunning impl
    if isJust cont
       then putStrLn ("Ending Simpleloop: " ++ show cont)
       else doLoop impl


-- |Create a new 'MainloopImpl' with initial state
getMainloopImpl :: IO MainloopImpl
getMainloopImpl = MainloopImpl
    <$> newIORef []
    <*> newIORef []
    <*> newIORef []
    <*> newIORef []

    <*> newIORef 0
    <*> newIORef 0
    <*> newIORef 0

    <*> newIORef Nothing
    <*> createPipe
    --    <*> do
    --        (r, w) <- createPipe
    --        rh <- fdToHandle r
    --        wh <- fdToHandle w
    --        return (rh, wh)


atomModifyIORef :: IORef a -> (a -> a) -> IO ()
atomModifyIORef ref fun = void $ atomicModifyIORef ref (fun &&& id)

removeTimeEvent :: TimeEvent -> IO ()
removeTimeEvent evt =
    atomModifyIORef (implTimeEvents . timeImpl $ evt) (deleteBy (\x y -> snd x == snd y) (dummyTime, evt))

wakeImpl :: MainloopImpl -> IO ()
wakeImpl = void . flip fdWrite "wakeup" . snd . implPipe

instance PAMainloop MainloopImpl where
    -- newtype wrappers, only needed because of the type system limitations
    newtype PAIOEvent    MainloopImpl = PAIOEvent IOEvent
    newtype PATimeEvent  MainloopImpl = PATimeEvent TimeEvent
    newtype PADeferEvent MainloopImpl = PADeferEvent DeferEvent

    ioNew    :: MainloopImpl -> Fd -> [PAIOEventFlags] -> ([PAIOEventFlags] -> IO ()) -> IO (PAIOEvent MainloopImpl)
    ioNew impl fd flags callback = do
        count <- atomicModifyIORef (implIOCount impl) ((+1) &&& id)
        -- putStrLn ("new IOEvent: " ++ show count)
        evt <- IOEvent callback fd impl count <$> newIORef flags <*> newIORef (return ())
        atomModifyIORef (implIOEvents impl) (evt:)
        wakeImpl impl
        return $ PAIOEvent evt

    ioEnable :: (PAIOEvent MainloopImpl) -> [PAIOEventFlags] -> IO ()
    ioEnable (PAIOEvent x) flags = do
        atomicWriteIORef (ioEvents x) flags
        wakeImpl $ ioImpl x

    ioFree   :: (PAIOEvent MainloopImpl) -> IO ()
    ioFree (PAIOEvent x) = do
        -- putStrLn "removing IOEvent"
        atomModifyIORef (implIOEvents . ioImpl $ x) (delete x)
        join . readIORef . ioDestroy $ x
        wakeImpl $ ioImpl x

    ioSetDestroy :: (PAIOEvent MainloopImpl) -> IO () -> IO ()
    ioSetDestroy (PAIOEvent x) = atomicWriteIORef (ioDestroy x)


    timeNew :: MainloopImpl -> PATime -> (PATime -> IO ()) -> IO (PATimeEvent MainloopImpl)
    timeNew impl time callback = do
        -- putStrLn ("new TimeEvent: " ++ show time)
        count <- atomicModifyIORef (implTimeCount impl) ((+1) &&& id)
        evt <- TimeEvent callback impl count <$> newIORef time <*> newIORef (return ())
        atomModifyIORef (implTimeEvents impl) (insertBy (comparing fst) (time, evt))
        wakeImpl impl
        return $ PATimeEvent evt

    timeRestart :: PATimeEvent MainloopImpl -> PATime -> IO ()
    timeRestart (PATimeEvent evt) time = do
        -- putStrLn ("restarting Timer" ++ show time)
        removeTimeEvent evt
        writeIORef (timeDeadline evt) time
        atomModifyIORef (implTimeEvents $ timeImpl evt) (insertBy (comparing fst) (time, evt) . (filter ((/= evt) . snd)))
        atomModifyIORef (implTimeDisable $ timeImpl evt) (filter (/=evt))
        wakeImpl $ timeImpl evt

    timeFree :: PATimeEvent MainloopImpl -> IO ()
    timeFree (PATimeEvent x) = do
        -- putStrLn "removing Timer"
        removeTimeEvent x
        atomicWriteIORef (timeDeadline x) dummyTime
        join . readIORef . timeDestroy $ x
        wakeImpl $ timeImpl x

    timeSetDestroy :: PATimeEvent MainloopImpl -> IO () -> IO ()
    timeSetDestroy (PATimeEvent x) = atomicWriteIORef (timeDestroy x)


    deferNew :: MainloopImpl -> IO () -> IO (PADeferEvent MainloopImpl)
    deferNew impl callback = do
        -- putStrLn "new DeferEvent"
        count <- atomicModifyIORef (implDeferCount impl) ((+1) &&& id)
        evt <- DeferEvent callback impl count <$> newIORef True <*> newIORef (return ())
        atomModifyIORef (implDeferEvents impl) (evt:)
        wakeImpl impl
        return $ PADeferEvent evt

    deferEnable :: PADeferEvent MainloopImpl -> Bool -> IO ()
    deferEnable (PADeferEvent x) = atomicWriteIORef (deferEnabled x)

    deferFree :: PADeferEvent MainloopImpl -> IO ()
    deferFree (PADeferEvent x) = do
        -- putStrLn "removing DeferEvent"
        atomModifyIORef (implDeferEvents . deferImpl $ x) (delete x)
        join . readIORef . deferDestroy $ x
    -- wakeImpl impl -- not here, when removed it doesn't matter and we
    -- shouldn't block while a defere event exists

    deferSetDestroy :: PADeferEvent MainloopImpl -> IO () -> IO ()
    deferSetDestroy (PADeferEvent x) = atomicWriteIORef (deferDestroy x)

    quitLoop :: MainloopImpl -> Int -> IO ()
    quitLoop impl = atomicWriteIORef (implRunning impl) . Just


{-# LANGUAGE ForeignFunctionInterface #-}
import Sound.Pulse.Context
import Sound.Pulse.Mainloop.Simple

import Data.Word (Word32)
import Control.Monad (void)
import Sound.Pulse.Mainloop
import Sound.Pulse.Sinkinfo
import Sound.Pulse.Subscribe
import Sound.Pulse.Serverinfo
import Sound.Pulse.Volume

printSink :: Sinkinfo -> IO ()
printSink sink = do
    let vol = cVolumeToLinear $ siVolume sink
    let base = volumeToLinear $ siBaseVolume sink
    putStrLn . show $ map (\v -> v / base * 100) vol

startLoop :: Context -> Sinkinfo -> IO ()
startLoop cxt info = do 
    printSink info
    void $ subscribeEvents cxt [SubscriptionMaskSink] fun
    where fun :: ((SubscriptionEventFacility, SubscriptionEventType) -> Word32 -> IO ())
          fun _ 0 = void $ getContextSinkByIndex cxt (siIndex info) printSink
          fun _ _ = return ()

getDefaultSink :: Context -> IO ()
getDefaultSink cxt = void $ getServerInfo cxt fun
    where fun :: ServerInfo -> IO ()
          fun serv = let name = defaultSinkName serv in
                         do
                            putStrLn ("Default sink: " ++ name)
                            void $ getContextSinkByName cxt name (startLoop cxt)



main :: IO ()
main = do
    impl <- getMainloopImpl
    cxt <- getContext impl "hs-test"
    setStateCallback cxt $ do
        state <- getContextState cxt
        putStrLn ("State: " ++ show state)
        case state of
            ContextFailed -> do
                putStr "PulseError: "
                putStrLn =<< getContextErrStr cxt
                quitLoop impl =<< getContextErr cxt
            ContextReady -> getDefaultSink cxt
            _ -> return ()
    connectContext cxt Nothing []
    doLoop impl

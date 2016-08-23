{-# LANGUAGE ForeignFunctionInterface #-}
import Sound.Pulse.Context
import Sound.Pulse.Mainloop.Simple

import Control.Monad (void)
import Sound.Pulse.Mainloop
import Sound.Pulse.Sinkinfo
import Sound.Pulse.Subscribe

dumpSinks :: Context -> IO ()
dumpSinks cxt = void $ getContextSinks cxt fun endf
    where fun = putStrLn . show
          -- endf = quitLoop impl 0
          endf = void $ subscribeEvents cxt [SubscriptionMaskAll] subFun
          subFun x y = putStrLn ("Event: " ++ show x ++ " with idx: " ++ show y)

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
            ContextReady -> dumpSinks cxt
            _ -> return ()
    connectContext cxt Nothing []
    --connectPAContext cxt (Just "10.13.36.2") []
    doLoop impl

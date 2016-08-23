{-# LANGUAGE ForeignFunctionInterface #-}
import Sound.Pulse.Context
import Sound.Pulse.Mainloop.Simple

import Control.Monad (void)
import Sound.Pulse.Mainloop
import Sound.Pulse.Sinkinfo
import Sound.Pulse.Subscribe

dumpSinks :: PAContext -> IO ()
dumpSinks cxt = void $ getContextSinks cxt fun endf
    where fun = putStrLn . show
          -- endf = quitLoop impl 0
          endf = void $ subscribeEvents cxt [SubscriptionMaskAll] subFun
          subFun x y = putStrLn ("Event: " ++ show x ++ " with idx: " ++ show y)

main :: IO ()
main = do
    impl <- getMainloopImpl
    cxt <- getPAContext impl "hs-test"
    setPAStateCallback cxt (\ccxt {- This is pretty useles, but who gives -}-> do
        state <- getPAContextState ccxt
        putStrLn ("State: " ++ show state)
        case state of
            PAContextFailed -> do
                putStr "PulseError: "
                putStrLn =<< getPAContextErrStr ccxt
                quitLoop impl =<< getPAContextErr ccxt
            PAContextReady -> dumpSinks cxt
            _ -> return ()
        )
    connectPAContext cxt Nothing []
    --connectPAContext cxt (Just "10.13.36.2") []
    doLoop impl

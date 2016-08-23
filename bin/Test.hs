{-# LANGUAGE ForeignFunctionInterface #-}
import Sound.Pulse.Context
import Sound.Pulse.Mainloop.Simple

import Sound.Pulse.Mainloop
import Sound.Pulse.Sinkinfo

dumpSinks :: PAMainloop a => a -> PAContext -> IO ()
dumpSinks impl cxt = getContextSinks cxt fun endf
    where fun = putStrLn . show
          endf = quitLoop impl 0

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
            PAContextReady -> dumpSinks impl cxt
            _ -> return ()
        )
    connectPAContext cxt Nothing []
    --connectPAContext cxt (Just "10.13.36.2") []
    doLoop impl

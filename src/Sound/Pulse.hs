{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
module Sound.Pulse
    ( Pulse (..)
    , runPulse
    , runPulse_
    , runGetPulse

    , pulseListM
    )
where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar, newMVar, modifyMVar_)
import Sound.Pulse.Context (Context)

data Pulse a = Pulse (Context -> (a -> IO ()) -> IO ())

instance Functor Pulse where
    fmap :: (a -> b) -> Pulse a -> Pulse b
    fmap f (Pulse x) =
        let g cxt y = x cxt (y . f)
         in Pulse g

instance Applicative Pulse where
    pure :: a -> Pulse a
    pure x = Pulse (\_ f -> f x)

    (<*>) :: Pulse (a -> b) -> Pulse a -> Pulse b
    (Pulse f0) <*> (Pulse g0) =
        let g1 cxt h t = g0 cxt (h . t)
         in Pulse (\cxt h -> f0 cxt (g1 cxt h))

instance Monad Pulse where
    return :: a -> Pulse a
    return = pure

    (>>=) :: Pulse a -> (a -> Pulse b) -> Pulse b
    (Pulse a) >>= f =
        Pulse (\cxt g -> a cxt (\v -> let Pulse h = f v in h cxt g))

instance MonadIO Pulse where
    liftIO :: IO a -> Pulse a
    liftIO a = Pulse (\_ f -> f =<< a)

runPulse :: MonadIO m => Context -> Pulse a -> (a -> IO ()) -> m ()
runPulse cxt (Pulse x) f = liftIO $ x cxt f

runPulse_ :: MonadIO m => Context -> Pulse a -> m ()
runPulse_ cxt (Pulse x) = liftIO $ x cxt (const $ return ())

runGetPulse :: MonadIO m => Context -> Pulse a -> m a
runGetPulse cxt x = liftIO $ do
    var <- newEmptyMVar
    runPulse cxt x (putMVar var)
    takeMVar var

pulseListM :: (Context -> (a -> IO ()) -> IO () -> IO ()) -> Pulse [a]
pulseListM fun = Pulse $ \cxt f -> do
    var <- newMVar []
    let cb v = modifyMVar_ var $ return . (v:)
    fun cxt cb (f . reverse =<< takeMVar var)


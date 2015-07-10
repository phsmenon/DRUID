{-# LANGUAGE TupleSections #-}
module Druid.Combinators where

import Data.Maybe
import Data.IORef
import Debug.Trace
import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Time.Clock

import Druid.EngineMonad

------------------------------------------------------
-- Helper Types
------------------------------------------------------

-- This class represents values that can be scaled
class Num a => Vec a where
  (*^) :: Double -> a -> a

instance Vec Double where
  (*^) = (*)

-----------------------------------------------------------------
-- Basic lifters
-----------------------------------------------------------------

lift0 :: Monad  m => a -> SignalC m a
lift0 v = ConstSignalC v

lift1 :: Monad m => (a -> b) -> SignalC m a -> SignalC m b
lift1 fn sig = f sig where
  f a = SignalC $ \s -> do (a', v) <- stepSignal s a 
                           return (f a', fn v)

lift2 :: Monad m => (a -> b -> c) -> SignalC  m a -> SignalC m b -> SignalC m c
lift2 fn sig1 sig2 = f sig1 sig2
  where f a1 a2 = SignalC $ \s -> do (a1', v1) <- stepSignal s a1
                                     (a2', v2) <- stepSignal s a2
                                     return (f a1' a2', fn v1 v2)

lift3 :: Monad m => (a -> b -> c -> d) -> SignalC m a -> SignalC m b -> SignalC m c -> SignalC m d
lift3 fn sig1 sig2 sig3 = f sig1 sig2 sig3
  where f a1 a2 a3 = SignalC $ \s -> do (a1', v1) <- stepSignal s a1
                                        (a2', v2) <- stepSignal s a2
                                        (a3', v3) <- stepSignal s a3
                                        return (f a1' a2' a3', fn v1 v2 v3)


-----------------------------------------------------------------
-- Tracing
-----------------------------------------------------------------

-- TODO: Switch the tracing to debug.trace so that the dependency on MonadIO can be removed

traceB :: (MonadIO m, Show a) => String -> SignalC m a -> SignalC m a
-- Print a signal/time, prefixed by the string
traceB text sig = f sig where
  f a = SignalC $ \s -> do (a', v) <- stepSignal s a
                           {-liftIO $ putStrLn $ text ++ (show v)-}
                           liftIO $ putStrLn $ text ++ (show s) ++ ": " ++ (show v)
                           return (f a', v)
                            
traceE :: (MonadIO m, Show a) => String -> EventC m a -> EventC m a
-- Print event value on the console when it occurs.
traceE text ev = f ev where
  f e = Event $ \s -> do (e', maybeV) <- stepEvent s e
                         let res = return (f e', maybeV)
                         maybe res (traceTextAndReturn res) maybeV
  traceTextAndReturn res v = liftIO ( putStrLn $ text ++ (show v) ) >>= return res

traceEX :: MonadIO m => String ->  EventC m a -> EventC m a
-- Print event value on the console when it occurs.
traceEX text ev = f ev where
  f e = Event $ \s -> do (e', maybeV) <- stepEvent s e
                         let res = return (f e', maybeV)
                         maybe res (const $ traceTextAndReturn res) maybeV
  traceTextAndReturn res = liftIO ( putStrLn text ) >>= return res

-----------------------------------------------------------------
-- Choices
-----------------------------------------------------------------

choose :: Monad m => SignalC m Bool -> SignalC m a -> SignalC m a -> SignalC m a
choose = lift3 (\test i e -> if test then i else e)

-----------------------------------------------------------------
-- Observation
-----------------------------------------------------------------

observe :: MonadIO m => SignalC m a -> m (SignalC m a)
-- Create an observing signal.  Use an IORef to hold the observed
-- value.  Make the built-in mouse function an observation.
observe sig = do ref <- liftIO $ newIORef Nothing
                 return $ f ref sig
  where
    f ref a = SignalC $ \s@(Stimulus t) -> do v <- liftIO $ readIORef ref
                                              case v of 
                                                Just (t', res) | t' == t -> return res
                                                x -> do (a', av) <- stepSignal s a
                                                        let res = (f ref a', av)
                                                        liftIO $ writeIORef ref $ Just (t, res)
                                                        {-liftIO . traceIO $ "Ran observed signal: " ++ (maybe "Nothing" (show . fst) x) ++ " " ++ (show t)-}
                                                        return res

-----------------------------------------------------------------
-- SignalC Constructors
-----------------------------------------------------------------

hold :: Monad m => a -> EventC m a -> SignalC m a
-- Create a signalc out of a stream of events
hold v0 ev = f v0 ev
  where f v e = SignalC $ \s -> do (e', val) <- stepEvent s e
                                   let val' = fromMaybe v val
                                   return (f val' e', val')


-----------------------------------------------------------------
-- SignalC Combinators
-----------------------------------------------------------------

integral :: Monad m => Vec a => SignalC m a -> SignalC m a
integral b = SignalC $ \s@(Stimulus t) ->let SignalC b' = inner b 0 t in b' s where
  inner a total tlast = SignalC $ \s@(Stimulus t) -> do (a', new) <- stepSignal s a
                                                        let total' = total + ((t - tlast) *^ new)
                                                        return (inner a' total' t, total')

-----------------------------------------------------------------
-- Event Constructors
-----------------------------------------------------------------
  
never :: Monad m => EventC m a
never = Event $ const . return $ (never, Nothing)

always :: Monad m => a -> EventC m a
always v = Event $ const . return $ (always v, Just v)
                                             
-----------------------------------------------------------------
-- Event Combinators
-----------------------------------------------------------------

mapEs :: Monad m => st -> ((st, a) -> (st, Maybe b)) -> EventC m a -> EventC m b
-- Helper method to create manage events
mapEs st0 fn ev = f st0 ev where
  f st e = Event $ \s -> do (e', val) <- stepEvent s e
                            let (st', val') = maybe (st, Nothing) (fn . (st,)) val
                            return (f st' e', val')
  
once :: Monad m => EventC m a -> EventC m a
-- Make this event fire only once
once ev = f ev False where
  -- Should not age base even if it has occurred
  f e occurred = Event $ \s -> if (occurred) 
                                 then return (f e True, Nothing)
                                 else stepEvent s e >>= \(e', v) -> return (f e' (isJust v), v)

when :: Monad m => SignalC m Bool -> EventC m ()
-- Create an event when this signalc switches to True
when sig = f sig where
  f a = Event $ \s ->do (a', v) <- stepSignal s a
                        let res = if v then Just () else Nothing
                        return (f a', res)

(-=>) :: Monad m => EventC m a -> b -> EventC m b
-- Map an event to another value
(-=>) a b = mapEs Nothing f a where
  f (_, _) = (Nothing, Just b)  
  
(==>) :: Monad m => EventC m a -> (a -> b) -> EventC m b
-- Map an event to another value by applying a function to it
(==>) a fn = mapEs Nothing f a where
  f (_, v) = (Nothing, Just $ fn v)

snap :: Monad m => EventC m a -> SignalC m b -> EventC m b
-- Map an event by snapshotting a signal
snap a b = f a b where
  f e sig = Event $ \s -> do (e', ev) <- stepEvent s e
                             case ev of
                               Nothing -> return (f e' sig, Nothing)
                               Just _  -> do (sig', v) <- stepSignal s sig
                                             return (f e' sig', Just v)
  
(.|.) :: Monad m => EventC m a -> EventC m a -> EventC m a
-- Choose the first event that occurred
(.|.) a b = f a b where
  f ea eb = Event $ \s -> do (ea', va') <- stepEvent s ea
                             case va' of 
                               Just _  -> return (f ea' eb, va')
                               Nothing -> do (eb', vb') <- stepEvent s eb
                                             return (f ea' eb', vb')

-----------------------------------------------------------------
-- Switchers
-----------------------------------------------------------------


switch :: Monad m => SignalC m a -> EventC m (m (SignalC m a)) -> SignalC m a
-- Switch to a new signalc *every time* an event occurs
switch a0 e0 = f a0 e0 where
  f :: Monad m => SignalC m a -> EventC m (m (SignalC m a)) -> SignalC m a
  f sig e = SignalC $ \s -> do (e', ev) <- stepEvent s e
                               (sig', v') <- fromMaybe (return sig) ev >>= stepSignal s
                               return (f sig' e', v')
                                                 
switchLater :: Monad m => SignalC m a -> EventC m (m (SignalC m a)) -> SignalC m a
-- Switch to a new signalc *every time* an event occurs
switchLater a0 e0 = f a0 e0 where
  f :: Monad m => SignalC m a -> EventC m (m (SignalC m a)) -> SignalC m a
  f sig e = SignalC $ \s -> do (e', ev) <- stepEvent s e
                               (sig', v') <- stepSignal s sig
                               fromMaybe (return sig') ev >>= \b' -> return (f b' e', v')

until :: Monad m => SignalC m a -> EventC m (m (SignalC m a)) -> SignalC m a
-- Switch to a new signalc *the first time* an event occurs
until a0 e0 = f a0 e0 where
  f :: Monad m => SignalC m a -> EventC m (m (SignalC m a)) -> SignalC m a
  f sig e = SignalC $ \s -> do (e', ev) <- stepEvent s e
                               if isJust ev 
                                then fromJust ev >>= stepSignal s
                                else stepSignal s sig >>= \(sig', v') -> return (f sig' e', v')

untilLater :: Monad m => SignalC m a -> EventC m (m (SignalC m a)) -> SignalC m a
-- Switch to a new signalc *the first time* an event occurs
untilLater a0 e0 = f a0 e0 where
  f :: Monad m => SignalC m a -> EventC m (m (SignalC m a)) -> SignalC m a
  f sig e = SignalC $ \s -> do (e', ev) <- stepEvent s e
                               (sig', v') <- stepSignal s sig
                               if isJust ev
                                then fromJust ev >>= \sig'' -> return (sig'', v')
                                else return (f sig' e', v')

accum :: Monad m => a -> EventC m (a -> a) -> SignalC m a
-- Standard accumulator - except that the event stream produces functions that 
-- helps compute new values based on the current value
accum initial a = f a initial where
  f e v = SignalC $ \s -> do (e', ev) <- stepEvent s e
                             let v' = maybe v ($ v) ev
                             return (f e' v', v')

------------------------------------------------------
-- Basic Signals
------------------------------------------------------

time :: Monad m => SignalC m Double
-- Current time as a signal. Requires a heartbeat to be any use.
time = f where
   f = SignalC (\(Stimulus t) -> return (f, t))

-----------------------------------------------------------------
-- Construction Helpers (primarily for toolkit bindings)
-----------------------------------------------------------------

nativeEvent :: Monad m => (Stimulus -> m (Maybe a)) -> EventC m a
nativeEvent fn = f where
  f = Event $ \s -> fn s >>= return . (f, )

nativeSignal :: Monad m => (Stimulus -> m a) -> SignalC m a
nativeSignal fn = f where
  f = SignalC $ \s -> fn s >>= return . (f, )

liftM1 :: Monad m => (a -> m b) -> SignalC m a -> SignalC m b
liftM1 fn a = f a where
  f sig = SignalC $ \s -> do (sig', v') <- stepSignal s sig
                             fn v' >>= return . (f sig', )

mapEsM :: Monad m => EventC m a -> (a -> m (Maybe b)) -> EventC m b
mapEsM ev fn = f ev where
  f e = Event $ \s -> do (e', v) <- stepEvent s e
                         v' <- maybe (return Nothing) fn v
                         return (f e', v')

------------------------------------------------------
-- Step Helper
------------------------------------------------------

stepSignal :: Monad m => Stimulus -> SignalC m a -> m (SignalC m a, a)
stepSignal st (SignalC f) = f st
stepSignal _ s@(ConstSignalC v) = return (s, v)

stepEvent :: Monad m => Stimulus -> EventC m a -> m (EventC m a, Maybe a)
stepEvent st (Event f) = f st

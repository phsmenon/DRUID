module Druid.Engine where

import Data.Maybe
import Data.IORef
import Debug.Trace
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Time.Clock

import Druid.Types
import Druid.DruidMonad

-----------------------------------------------------------------
-- Some Type Abbreviations
-----------------------------------------------------------------
--
type BD  = Behavior Double
type BB  = Behavior Bool
type BS  = Behavior String

-----------------------------------------------------------------
-- Basic lifters
-----------------------------------------------------------------

lift0 :: a -> Behavior a
lift0 v = f where
  f = Behavior $ const $ return (f, v)

lift1 :: (a -> b) -> Behavior a -> Behavior b
lift1 fn (Behavior a0) = f a0 where
  f a = Behavior $ \s -> do (Behavior a', av) <- a s 
                            return (f a', fn av)

lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
lift2 fn (Behavior a0) (Behavior b0) = f a0 b0
  where f a b = Behavior $ \s -> do (Behavior a', av) <- a s
                                    (Behavior b', bv) <- b s
                                    return (f a' b', fn av bv)

lift3 :: (a -> b -> c -> d) -> Behavior a -> Behavior b -> Behavior c -> Behavior d
lift3 fn (Behavior a0) (Behavior b0) (Behavior c0) = f a0 b0 c0
  where f a b c = Behavior $ \s -> do (Behavior a', av) <- a s
                                      (Behavior b', bv) <- b s
                                      (Behavior c', cv) <- c s
                                      return (f a' b' c', fn av bv cv)


-----------------------------------------------------------------
-- Tracing
-----------------------------------------------------------------


traceB :: Show a => String -> Behavior a -> Behavior a
-- Print a signal/time, prefixed by the string
traceB text (Behavior a0) = f a0 where
  f a = Behavior $ \s -> do (Behavior a', av) <- a s
                            liftIO $ putStrLn $ text ++ (show av)
                            return (f a', av) -- Do not change the age
							
traceE :: Show a => String -> Event a -> Event a
-- Print event value on the console when it occurs.
traceE text (Event a0) = f a0 where
  f a = Event $ \s -> do (Event a', ev) <- a s
                         case ev of 
                           Just v  -> do liftIO $ putStrLn $ text ++ (show v)
                                         return (f a', ev)
                           Nothing -> return (f a', ev)

-----------------------------------------------------------------
-- Choices
-----------------------------------------------------------------

choose :: BB -> Behavior a -> Behavior a -> Behavior a
choose = lift3 (\test i e -> if test then i else e)

-----------------------------------------------------------------
-- Observation
-----------------------------------------------------------------

-- TODO: FIX THIS
observe :: Behavior a -> Druid (Behavior a)
-- Create an observing behavior.  Use an IORef to hold the observed
-- value.  Make the built-in mouse function an observation.
observe (Behavior a0) = do ref <- liftIO $ newIORef Nothing
                           return $ f ref a0
  where
    f ref a = Behavior $ \s@(t,_) -> do v <- liftIO $ readIORef ref
                                        case v of 
                                          Just (t', res) | t' == t -> return res
                                          _ -> do (Behavior a', av) <- a s
                                                  let res = (f ref a', av)
                                                  liftIO $ writeIORef ref $ Just (t, res)
                                                  return res

-----------------------------------------------------------------
-- Behavior Constructors
-----------------------------------------------------------------

hold :: a -> Event a -> Behavior a
-- Create a behavior out of a stream of events
hold v0 (Event a0) = f v0 a0
  where f v a = Behavior $ \s -> do (Event e', ev) <- a s
                                    let val = fromMaybe v ev
                                    return (f val e', val)


-----------------------------------------------------------------
-- Behavior Combinators
-----------------------------------------------------------------

integral :: Vec a => Behavior a -> Behavior a
-- Create a progressively increasing (reactive) value
integral (Behavior a) = f a (0, 0) where
  f a (pt, pv) = Behavior $ \s@(t, ev) -> do (Behavior a', av) <- a s
                                             let val = pv + (t - pt) *^ av
                                             return (f a' (t, val), val)

-----------------------------------------------------------------
-- Event Constructors
-----------------------------------------------------------------
  
never :: Event a
never = Event $ const . return $ (never, Nothing)
											 
stimEvent :: (UIEvent -> Maybe v) -> Event v
stimEvent fn = f
  where f = Event(\(_, sev) -> return (f, maybe Nothing fn sev))
											 
-----------------------------------------------------------------
-- Event Combinators
-----------------------------------------------------------------
  
mapEs :: st -> ((st, a) -> (st, Maybe b)) -> Event a -> Event b
-- Helper method to create manage events
mapEs st0 f (Event a0) = f' st0 a0 where
  f' st a = Event $ \s -> do (Event a', ev) <- a s 
                             case ev of
                               Nothing -> return (f' st a', Nothing)
                               Just v -> do let (st', v') = f (st, v) 
                                            return (f' st' a', v')
once :: Event a -> Event a
-- Make this event fire only once
once e = mapEs False f e where
  f (occurred, v) = (True, if occurred then Nothing else Just v)

when :: Behavior Bool -> Event ()
-- Create an event when this behavior switches to True
when (Behavior b0) = f b0 where
  f b = Event $ \s ->do  (Behavior b', bv) <- b s
                         let res = if bv then Just () else Nothing
                         return (f b', res)

(-=>) :: Event a -> b -> Event b
-- Map an event to another value
(-=>) a b = mapEs Nothing f a where
  f (st, v) = (Nothing, Just b)  
  
(==>) :: Event a -> (a -> b) -> Event b
-- Map an event to another value by applying a function to it
(==>) a fn = mapEs Nothing f a where
  f (_, v) = (Nothing, Just $ fn v)

snap :: Event a -> Behavior b -> Event b
-- Map an event by snapshotting a behavior
snap a b = f a b where
  f (Event a) (Behavior b) = Event $ \s -> do (e', ev) <- a s
                                              (a', av) <- b s
                                              case ev of
                                                Nothing -> return (f e' a', Nothing)
                                                Just v  -> return (f e' a', Just av)
  
(.|.) :: Event a -> Event a -> Event a
-- Choose the first event that occurred
(.|.) a b = f a b where
  f ea@(Event a) eb@(Event b) = Event $ \s -> do (ea', eav') <- a s
                                                 case eav' of 
                                                   Just _  -> return (f ea' eb, eav')
                                                   Nothing -> do (eb', ebv') <- b s
                                                                 return (f ea' eb', ebv')
  
-----------------------------------------------------------------
-- Switchers
-----------------------------------------------------------------


switch :: Behavior a -> Event (Druid (Behavior a)) -> Behavior a
-- Switch to a new behavior *every time* an event occurs
switch a0 e0 = f a0 e0 where
  f :: Behavior a -> Event (Druid (Behavior a)) -> Behavior a
  f (Behavior a) (Event e) = Behavior $ \s -> do (ee', ev) <- e s
                                                 case ev of
                                                   Just b -> do Behavior newf <- b
                                                                (ba', av) <- newf s 
                                                                return (f ba' ee', av)
                                                   Nothing -> do (ba', av) <- a s 
                                                                 return (f ba' ee', av)
												 
untilB :: Behavior a -> Event (Druid (Behavior a)) -> Behavior a
-- Switch to a new behavior *the first time* an event occurs
untilB b e = switch b $ once e 

accum :: a -> Event (a -> a) -> Behavior a
-- Standard accumulator - except that the event stream produces functions that 
-- helps compute new values based on the current value
accum init a = f a init where
  f (Event a) v = Behavior $ \s -> do (e', ev) <- a s
                                      case ev of 
                                        Nothing  -> return (f e' v, v)
                                        Just f' -> do let val = f' v  
                                                      return (f e' val, val)

------------------------------------------------------
-- Basic Behaviors
------------------------------------------------------

time :: BD
-- Make the time value that comes as part of the stimulus in to
-- a behavior
time = f where
   f = Behavior (\(t, _) -> return (f, t))

clock :: Double -> Event ()
-- Create a clock that ticks with the given period
clock duration = f Nothing where
  f start = Event $ \(t', _) -> case start of
                                  Nothing -> return (f (Just t'), Nothing)
                                  Just t  -> if t' - t > duration then
                                               return (f (Just t'), Just ())
                                             else
                                               return (f start, Nothing)



------------------------------------------------------
-- Reactive Loop
------------------------------------------------------

type StepperData = IORef (Maybe (DruidData, Behavior (Druid ())))

doStep :: Behavior (Druid ()) -> UIEvent -> Druid (Behavior (Druid ()))
doStep (Behavior beh) event = do
  utcTime <- liftIO getCurrentTime
  let time = realToFrac $ utctDayTime utcTime :: Double
  (beh', op) <- beh (time, Just event)
  op
  return beh'

handleEvent :: StepperData -> UIEvent -> IO ()
handleEvent stepperData event = do
  dta <- readIORef stepperData
  case dta of 
    Nothing -> error "Internal error: stepperData should not be NULL inside handleEvent"
    Just (state, beh) -> do
      (beh', state') <- runStateT (doStep beh event) state
      writeIORef stepperData (Just (state', beh'))


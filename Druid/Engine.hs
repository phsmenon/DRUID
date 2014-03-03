{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Druid.Engine where

import Data.Maybe
import Data.IORef
import Debug.Trace
import Control.Applicative
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Time.Clock

import Druid.DruidMonad
--import Druid.Controls (createInternalTimer)

-----------------------------------------------------------------
-- Some Type Abbreviations
-----------------------------------------------------------------
--
type BD  = Behavior Double
type BB  = Behavior Bool
type BS  = Behavior String

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

lift0 :: a -> Behavior a
lift0 v = f where
  f = Behavior $ const $ return (f, v)

lift1 :: (a -> b) -> Behavior a -> Behavior b
lift1 fn (Behavior a0) = f a0 where
  f a = Behavior $ \s -> do (Behavior a', av) <- a s 
                            return (f a', fn av)

liftM1 :: (a -> Druid b) -> Behavior a -> Behavior b
liftM1 fn (Behavior a0) = f a0 where
  f a = Behavior $ \s -> do (Behavior a', av) <- a s 
                            val <- fn av
                            return (f a', val)

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

traceEX :: String ->  Event a -> Event a
-- Print event value on the console when it occurs.
traceEX text (Event a0) = f a0 where
  f a = Event $ \s -> do (Event a', ev) <- a s
                         case ev of 
                           Just v  -> do liftIO $ putStrLn text
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

observe :: Behavior a -> Druid (Behavior a)
-- Create an observing behavior.  Use an IORef to hold the observed
-- value.  Make the built-in mouse function an observation.
observe (Behavior a0) = do ref <- liftIO $ newIORef Nothing
                           return $ f ref a0
  where
    f ref a = Behavior $ \s@(Stimulus t) -> do v <- liftIO $ readIORef ref
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

{-integral :: Vec a => Behavior a -> Behavior a-}
{--- Create a progressively increasing (reactive) value-}
{-integral (Behavior a) = f a (0, 0) where-}
  {-f a (pt, pv) = Behavior $ \s@(t, ev) -> do (Behavior a', av) <- a s-}
                                             {-let val = pv + (t - pt) *^ av-}
                                             {-return (f a' (t, val), val)-}

integral :: Vec a => Behavior a -> Behavior a
integral b = Behavior $ \s@(Stimulus t) ->
    let Behavior b' = inner b 0 t
    in b' s

inner (Behavior b) sum tlast = Behavior $ \s@(Stimulus t) -> do
    (b', new) <- b s
    let sum' = sum + ((t - tlast) *^ new)
    return (inner b' sum' t, sum')

-----------------------------------------------------------------
-- Event Constructors
-----------------------------------------------------------------
  
never :: Event a
never = Event $ const . return $ (never, Nothing)

always :: a -> Event a
always v = Event $ const . return $ (always v, Just v)
                                             
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
once (Event a0) = f a0 False where
  -- Should not age base even if it has occurred
  f a occurred = Event $ \s -> if (occurred) then
                                 return (f a True, Nothing)
                               else
                                 a s >>= \(Event a', ev) -> return (f a' (isJust ev), ev)

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
                                                 
switchLater :: Behavior a -> Event (Druid (Behavior a)) -> Behavior a
-- Switch to a new behavior *every time* an event occurs
switchLater a0 e0 = f a0 e0 where
  f :: Behavior a -> Event (Druid (Behavior a)) -> Behavior a
  f (Behavior a) (Event e) = Behavior $ \s -> do (ee', ev) <- e s
                                                 case ev of
                                                   Just b -> do newf <- b
                                                                (ba', av) <- a s
                                                                return (f newf ee', av)
                                                   Nothing -> do (ba', av) <- a s 
                                                                 return (f ba' ee', av)

untilB :: Behavior a -> Event (Druid (Behavior a)) -> Behavior a
-- Switch to a new behavior *the first time* an event occurs
untilB b e = switchLater b $ once e 

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
time = f where
   f = Behavior (\(Stimulus t) -> return (f, t))

clock :: Double -> Event ()
-- Create a clock that ticks with the given period
clock duration = f Nothing where
  f start = Event $ \(Stimulus t') -> case start of
                                        Nothing -> return (f (Just t'), Nothing)
                                        Just t  ->  if t' - t > duration 
                                                    then
                                                      return (f (Just t'), Just ())
                                                    else
                                                      return (f start, Nothing)

-----------------------------------------------------------------
-- Guarded Actions
-----------------------------------------------------------------

reaction :: Event a -> (a -> Druid ()) -> Behavior ()
reaction e action = f e where
  f (Event a) = Behavior $ \s -> do (e', ev') <- a s
                                    maybe (return ()) action ev'
                                    return (f e', ())
  
------------------------------------------------------
-- Reactive Loop
------------------------------------------------------

stepBehavior :: Stimulus -> Behavior a -> Druid (Behavior a, a)
stepBehavior st (Behavior f) = f st

-- This is a bit strange because of 2 things:
--   1. Events can evolve (though that had be somewhat odd)
--   2. Reactors, while executing, can add other reactors
runReactors :: Stimulus -> Druid ()
runReactors st = do
  current <- getReactors 
  clearReactors
  next' <- mapM (\r -> stepBehavior st r >>= return . fst) current
  addReactors next'

-- Take a single step with the engine
stepEngine :: Stimulus -> Druid ()
stepEngine st = do
  updateAllReactives >>  runReactors st >>  doDeferredOps >> updateAllReactives
  where
    updateAllReactives = getReactives >>= mapM_ (\(AnyReactive r) -> updateAttributes st r)

-- Version of engine stepping that can be called from an IO monad
stepEngineIO :: StepperDataRef -> IO ()
stepEngineIO ref = do
  dta <- readIORef ref
  case dta of 
    Nothing -> error "Internal error: stepperData should not be NULL inside handleEvent"
    Just st -> do
      (_, st') <- runDruid doStep st
      writeIORef ref (Just st')
  where
    doStep = makeStimulus >>= stepEngine
    makeStimulus = do
      utcTime <- liftIO getCurrentTime
      let tm = realToFrac $ utctDayTime utcTime :: Double
      return $ Stimulus tm

-- Engine startup. Performes some bootstrapping
startEngine :: Druid() -> IO ()
startEngine startup = do
  druidData <- initializeDruidData
  let ref = stepperDataRef druidData
  let druidData' = druidData { reactors = [initialAction] }
  writeIORef ref $ Just druidData'
  stepEngineIO $ stepperDataRef druidData'
  where
    initialAction = reaction (once $ always ()) (\_ -> startup)

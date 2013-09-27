{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Druid.Engine where

import Data.Maybe
import Data.IORef
import Debug.Trace
import Control.Applicative
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Time.Clock

import Druid.Types
import Druid.DruidMonad
import Druid.Controls (createInternalTimer)
import qualified Graphics.UI.WX as WX

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

{-integral :: Vec a => Behavior a -> Behavior a-}
{--- Create a progressively increasing (reactive) value-}
{-integral (Behavior a) = f a (0, 0) where-}
  {-f a (pt, pv) = Behavior $ \s@(t, ev) -> do (Behavior a', av) <- a s-}
                                             {-let val = pv + (t - pt) *^ av-}
                                             {-return (f a' (t, val), val)-}

integral :: Vec a => Behavior a -> Behavior a
integral b = Behavior $ \s@(t, _) ->
    let Behavior b' = inner b 0 t
    in b' s

inner (Behavior b) sum tlast = Behavior $ \s@(t, _) -> do
    (b', new) <- b s
    let sum' = sum + ((t - tlast) *^ new)
    return (inner b' sum' t, sum')

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
once (Event a0) = f a0 False where
  f a occurred = Event $ \s -> if (occurred) then   -- Don't even bother to age the base event if it has already occurred
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

stepBehavior :: Behavior (Druid ()) -> UIEvent -> Druid (Behavior (Druid ()))
stepBehavior (Behavior beh) event = do
  -- Get the current time value
  utcTime <- liftIO getCurrentTime
  let time = realToFrac $ utctDayTime utcTime :: Double
  -- Perform any pending actions from previous step
  lastStep <- getTimeStep
  if lastStep < time then do doOps else return ()
  -- "Age" the behavior and make sure the side effects execute
  (beh', op) <- beh (time, Just event)
  op
  -- Return the new behavior
  return beh'

standardEventReceiver :: Druid (UIEvent -> IO())
standardEventReceiver = do
  ref <- get >>= return . stepperDataRef 
  return $ handleEvent ref

handleEvent :: StepperDataRef -> UIEvent -> IO ()
handleEvent stepperData event = do
  dta <- readIORef stepperData
  case dta of 
    Nothing -> error "Internal error: stepperData should not be NULL inside handleEvent"
    Just (state, beh) -> do
      (beh', state') <- runDruid (stepBehavior beh event) state
      writeIORef stepperData (Just (state', beh'))


startEngine :: Druid(Behavior (Druid ())) -> IO ()
startEngine behavior = do
  druidData <- initializeDruidData -- behavior
  -- Get the behavior out of the monad and reify changes immediately
  let timerAction = standardEventReceiver >>= (createInternalTimer 75)
  (beh', druidData') <- runDruid (behavior <* doOps <* timerAction) druidData 
  writeIORef (stepperDataRef druidData') $ Just (druidData', beh')
  
---------------------------------------------------------------

-- Make most methods in Num reactive

instance Num a => Num (Behavior a) where
  (+)           = lift2 (+)
  (-)           = lift2 (-)
  (*)           = lift2 (*)
  abs           = lift1 abs
  negate        = lift1 negate
  fromInteger i = lift0 (fromInteger i)
  signum        = error "Cant use signum on behaviors"
  
-- Make methods in Fractional reactive

instance Fractional a => Fractional (Behavior a)
  where
    (/) = lift2 (/)
    fromRational r = lift0 (fromRational r)

-- This class represents values that can be scaled
--
instance Num (WX.Point) where
   WX.Point x1 y1 + WX.Point x2 y2  = WX.Point (x1+x2) (y1+y2)
   WX.Point x1 y1 - WX.Point x2 y2  = WX.Point (x1-x2) (y1-y2)
   negate (WX.Point x y)      = WX.Point (-x) (-y)
   (*)                  = error "No * method for WX.Point"
   abs                  = error "No abs method for WX.Point"
   signum               = error "No * method for WX.Point"
   fromInteger 0        = WX.Point 0 0
   fromInteger _        = error "Only the constant 0 can be used as a WX.Point"

class Num a => Vec a where
  (*^) :: Double -> a -> a

instance Vec Double where
  (*^) = (*)

instance Vec WX.Point where
  d *^ (WX.Point x y) = WX.Point (round $ d*x') (round $ d*y')
    where x' = fromIntegral x
          y' = fromIntegral y

p2 :: Behavior Int -> Behavior Int -> Behavior (WX.Point)
p2 = lift2 WX.point

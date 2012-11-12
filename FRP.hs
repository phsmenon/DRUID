module Main where

-- import SOE
--
import qualified Graphics.UI.WX as WX hiding ((:=))
import Graphics.UI.WX( Prop( (:=) ) )

import System.Clock
import Data.IORef

import Data.Maybe
import Debug.Trace

type WidgetId = String

data Widget = Button WidgetId (WX.Button ()) |
              Label WidgetId (WX.StaticText ()) |
              Frame WidgetId (WX.Frame ())
    deriving (Show, Eq)

data UIEvent = Initialize | Command Widget
    deriving (Show, Eq)


type Stimulus = (Double, Maybe UIEvent)

data Behavior a = Behavior (Stimulus -> (Behavior a, a))

data Event a = Event (Stimulus -> (Event a, Maybe a))

-- Abbrevations for common behavior types

type BD  = Behavior Double
type BB  = Behavior Bool
type BS  = Behavior String

type BIO a = Behavior (IO a)

-- Basic lifters

lift0 :: a -> Behavior a
lift0 v = f
  where f = Behavior (const (f, v))

lift1 :: (a -> b) -> Behavior a -> Behavior b
lift1 fn (Behavior a0) = f a0
  where f a = Behavior (\s -> let (Behavior a', av) = a s in
                                (f a', fn av))

lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
lift2 fn (Behavior a0) (Behavior b0) = f a0 b0
  where f a b = Behavior (\s -> let (Behavior a', av) = a s in
                                let (Behavior b', bv) = b s in
                                  (f a' b', fn av bv))

lift3 :: (a -> b -> c -> d) -> Behavior a -> Behavior b -> Behavior c -> Behavior d
lift3 fn (Behavior a0) (Behavior b0) (Behavior c0) = f a0 b0 c0
  where f a b c = Behavior (\s -> let (Behavior a', av) = a s in
                                  let (Behavior b', bv) = b s in
                                  let (Behavior c', cv) = c s in
                                    (f a' b' c', fn av bv cv))

-- Numeric Lifting and the P2 class

intToDouble :: Int -> Double
intToDouble i = fromIntegral $ toInteger i

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
 
-- Create a class to represent a 2-D point

data P2 = P2 Double Double deriving (Show, Eq)

p2FromInt :: Int -> Int -> P2 
p2FromInt x y = P2 (fromIntegral x) (fromIntegral y)

-- Some of these methods don't work but it's better to make P2 an
-- instance of Num than to create a different class for operators
-- on P2 objects.

instance Num P2 where
   P2 x1 y1 + P2 x2 y2  = P2 (x1+x2) (y1+y2)
   P2 x1 y1 - P2 x2 y2  = P2 (x1-x2) (y1-y2)
   negate (P2 x y)      = P2 (-x) (-y)
   (*)                  = error "No * method for P2"
   abs                  = error "No abs method for P2"
   signum               = error "No * method for P2"
   fromInteger 0        = P2 0 0
   fromInteger _        = error "Only the constant 0 can be used as a P2"

-- Make a reactive constructor

-- Reactive getter functions

-- Use * to make reactive versions of Ord operators

(>*), (<*), (>=*), (<=*), (==*) :: BD -> BD -> BB
(>*)  = lift2 (>)
(<*)  = lift2 (<)
(>=*) = lift2 (>=)
(<=*) = lift2 (<=)
(==*) = lift2 (==)

-- Reactive and / or

(&&*), (||*) :: BB -> BB -> BB
(&&*) = lift2 (&&)
(||*) = lift2 (||)

-- This class represents values that can be scaled

class Num a => Vec a where
  (*^) :: Double -> a -> a

instance Vec Double where
  (*^) = (*)

instance Vec P2 where
  d *^ (P2 x y) = P2 (d*x) (d*y)

-- Use "choose" as a reactive if statement

choose :: BB -> Behavior a -> Behavior a -> Behavior a
choose = lift3 (\test i e -> if test then i else e)

-- Reactive string operators

(+++) :: BS -> BS -> BS
(+++) = lift2 (++)

showB :: Show a => Behavior a -> BS
showB = lift1 show

-- Basic Behaviors

time :: BD
time = f where
   f = Behavior (\(t, _) -> (f, t))

-- You can integrate Doule and P2

integral :: Vec a => Behavior a -> Behavior a
integral (Behavior a) = f a (0, 0) where
  f a (pt, pv) = Behavior(\s@(t, ev) -> let (Behavior a', av) = a s in
                                        let val = pv + (t - pt) *^ av in
                                          (f a' (t, val), val))

-- Find events in the stimulus stream.  This is a utility
-- to use for writing other GUI functionality

-- Event stream combinators

never :: Event a
never = Event (const (never, Nothing))

-- This internal function will allow you to build
-- most of the event functionality

mapEs :: st -> ((st, a) -> (st, Maybe b)) -> Event a -> Event b
mapEs st0 f (Event a0) = f' st0 a0 where
  f' st a = Event (\s -> let (Event a', ev) = a s in
                           case ev of
                             Nothing -> (f' st a', Nothing)
                             Just v -> let (st', v') = f (st, v) in (f' st' a', v'))

filterE :: Event a -> (a -> Bool) -> Event a
filterE = error "filterE not implemented"

-- This suppresses events once the tag supply is exhausted

tags :: [a] -> Event b -> Event a
tags = error "tags not implemented"

(==>) :: Event a -> (a -> b) -> Event b
(==>) a fn = mapEs Nothing f a where
  f (_, v) = (Nothing, Just $ fn v)

when :: Behavior Bool -> Event ()
when = error "when not implemented"


once :: Event a -> Event a
once = error "once not implemented"

clock :: Double -> Event ()
clock duration = f Nothing where
  f start = Event(\(t', e) -> case start of
                                Nothing -> (f (Just t'), Nothing)
                                Just t  -> if t' - t > duration then
                                             (f (Just t'), Just ())
                                           else
                                             (f start, Nothing))

(-=>) :: Event a -> b -> Event b
(-=>) a b = mapEs Nothing f a where
  f (st, v) = (Nothing, Just b)

(.|.) :: Event a -> Event a -> Event a
(.|.) a b = f a b where
  f :: Event a -> Event a -> Event a
  f ea@(Event a) eb@(Event b) = Event(\s -> let (ea', eav') = a s in
                                              case eav' of 
                                                Just _  -> (f ea' eb, eav')
                                                Nothing -> let (eb', ebv') = b s in
                                                            (f ea' eb', ebv'))


snap :: Event a -> Behavior b -> Event b
snap a b = f a b where
  f (Event a) (Behavior b) = Event(\s -> let (e', ev) = a s in
                                         let (a', av) = b s in
                                           case ev of
                                             Nothing -> (f e' a', Nothing)
                                             Just v  -> (f e' a', Just av))

-- Switchers

switch :: Behavior a -> Event (Behavior a) -> Behavior a
switch a0 e0 = f a0 e0 where
  f :: Behavior a -> Event (Behavior a) -> Behavior a
  f (Behavior a) (Event e) = Behavior (\s -> let (ee', ev) = e s in
                                               case ev of
                                                 Just (Behavior newf) -> let (ba', av) = newf s in (f ba' ee', av)
                                                 Nothing -> let (ba', av) = a s in (f ba' ee', av))


until :: Behavior a -> Event b -> (b -> Behavior a) -> Behavior a
until = error "until not implemented"
until_ :: Behavior a -> Event b -> Behavior a -> Behavior a
until_ = error "until_ not implemented"

accum :: a -> Event (a -> a) -> Behavior a
accum init a = f a init where
  f (Event a) v = Behavior(\s -> let (e', ev) = a s in
                                   case ev of 
                                     Nothing  -> (f e' v, v)
                                     Just f' -> let val = f' v in (f e' val, val)) 

hold :: a -> Event a -> Behavior a
hold v0 (Event a0) = f v0 a0
  where f v a = Behavior (\s -> let (Event e', ev) = a s in
                              let val = fromMaybe v ev in
                                (f val e', val))


-- Graphics operations

{-animate :: BG -> IO ()-}
{-animate (Behavior b)-}
   {-= runGraphics $ do-}
                                     {--- Window location and size-}
        {-w <- openWindowEx "FRP Test" (Just (0,0)) (Just (500, 500))-}
               {-drawBufferedGraphic-}
        {-t0 <- timeGetTime-}
        {-ws <- getWindowSizeGL w-}
        {-let -}
          {-run b e = do-}
            {-t <- timeGetTime-}
            {-let ft = intToDouble (word32ToInt (t-t0)) / 1000-}
            {-let (Behavior b1, g) = b (ft, e)-}
            {-setGraphic w g-}
            {-case e of-}
              {-Nothing -> l b1-}
              {-Just Closed -> closeWindow w-}
              {-_ -> do putStrLn (show e)-}
                      {-l b1-}

          {-l b = do-}
            {-e <- maybeGetWindowSOEEvent w-}
            {-run b e     -}
        {-run b (Just $ Resize ws)-}

stimEvent :: (UIEvent -> Maybe v) -> Event v
stimEvent fn = f
  where f = Event(\(_, sev) -> (f, maybe Nothing fn sev))

onCommand :: Widget -> Event ()
onCommand widget  = stimEvent f where
  f (Command w) | w == widget = Just ()
  f _ = Nothing

getFrame :: Widget -> WX.Frame ()
getFrame (Frame id f) = f


makeButton :: Widget -> WidgetId -> String -> IORef (Maybe (BIO ())) -> IO Widget
makeButton parent id text bref = do 
  b <- WX.button (getFrame parent) [WX.text := text]
  let widget = Button id b
  WX.set b [ WX.on WX.command := handleCommand widget bref ]
  return widget

makeLabel :: Widget -> WidgetId -> String -> IORef (Maybe (BIO ())) -> IO Widget
makeLabel parent id text bref = do 
  l <- WX.staticText (getFrame parent) [WX.text := text]
  return $ Label id l

makeFrame :: WidgetId -> String -> IORef (Maybe (BIO ())) -> IO Widget
makeFrame id text bref = do 
  f <- WX.frame [WX.text := text]
  return $ Frame id f 

handleCommand :: Widget -> IORef (Maybe (BIO ()))-> IO ()
handleCommand w beh = do
  bio <- readIORef beh
  if isNothing bio then
    error "Behavior is NULL??"
  else do
    bio' <- step (fromJust bio) (Command w)
    writeIORef beh (Just bio')

step :: BIO () -> UIEvent -> IO (BIO ())
step (Behavior b) e = do
  TimeSpec x y <- getTime Realtime
  let (b', io) = b ((fromIntegral x), Just e)
  io
  return b'
  
main :: IO ()
main = do
  WX.start $ gui 

str2Int :: String -> Int  
str2Int s = (read s) :: Int


strInc :: String -> String
strInc s = show $  ( (read s) :: Int )  + 1

widgetLayout :: Widget -> WX.Layout
widgetLayout (Label id l) = WX.widget l
widgetLayout (Button id b) = WX.widget b

setText :: Widget -> String -> IO ()
setText (Label id l) text = WX.set l [WX.text := text]
setText (Button id b) text = WX.set b [WX.text := text]

setLayout :: Widget -> WX.Layout -> IO ()
setLayout (Frame id f) layout = WX.windowSetLayout f layout

gui :: IO ()
gui = do  
  bref <- newIORef Nothing

  f <- makeFrame "FRAME1" "Counter" bref
  l <- makeLabel f "LABEL1" "" bref
  b <- makeButton f "BUTTON1" "Count" bref

  let layout = WX.minsize (WX.sz 300 300) $ WX.row 5 [WX.hstretch $ widgetLayout l, widgetLayout b]
  setLayout f layout

  let initialBehavior = lift1 (setText l) $ accum "0" (onCommand b ==> (\_ -> strInc))
  start bref initialBehavior

start :: IORef (Maybe (BIO ())) -> BIO () -> IO ()
start bref bio = do
  writeIORef bref (Just bio)
  return ()

---------------------------------------------------------------------------------        
-- Test
---------------------------------------------------------------------------------        

-- let beh = switch (lift1 [set l [text := "Init"]]) (onCommand b) (lift1 [set l [text := "Foo"]])

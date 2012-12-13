module Main where

import Druid.DruidMonad
import Druid.Engine
import qualified Druid.EngineM as EM
import Druid.Controls

import Druid.ReactiveUI

import Graphics.UI.WX hiding (Button, Frame, Panel, Widget, Event)
import qualified Graphics.UI.WX as WX

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import System.Exit
import System.Environment(getArgs)

import System.Random

guiA :: Druid(Behavior (Druid ()))
guiA = do
  f <- createFrame [text := "Click Count", on closing := exitSuccess]
  l <- createLabel f [text := "Count"]
  b <- createButton f [text := "Press Me"]
  setProperties f [size := sz 150 50]
  setProperties l [fontSize := 20]
  setProperties b [position := point 50 0]
  makeBehavior f l b
  where
    makeBehavior :: Frame -> Label -> Button -> Druid(Behavior (Druid ()))
    makeBehavior f l b = do
      event <- onCommand b
      let beh = accum (0::Integer) (event  -=> (1 +)) 
      let beh' = lift1 show beh |-> (l, text)
      return beh'

-- Animating button
guiAnimatingButton :: Druid(Behavior (Druid ()))
guiAnimatingButton = do
  f <- createFrame [text := "Escaping Button", on closing := exitSuccess]
  b <- createButton f [text := "Help Me"]
  setProperties f [size := sz 300 200]
  setProperties b [position := point 50 500, size := sz 120 10]
  makeBehavior f b
  where
    makeBehavior :: Frame -> Button -> Druid(Behavior (Druid ()))
    makeBehavior f b = do
      event <- onCommand b
      let x = (p2 50 50) + integral (p2 50 0)
          beh = switch ((p2 50 50) |-> (b, position)) 
                  (event -=> return (x |-> (b, position) $$ 
                    (lift0 "Later Sucker" |-> (b, text))))
      return beh

guiC :: Druid(Behavior (Druid ()))
guiC = do
  f <- createFrame [text := "Timing", on closing := exitSuccess]
  l <- createLabel f [text := "Count"]
  s <- createSpin f 1 5 []
  setProperties f [size := sz 150 50]
  setProperties s [position := point 0 30]
  setProperties l [fontSize := 20]
  makeBehavior f l s
  where
    makeBehavior :: Frame -> Label -> Spin -> Druid(Behavior (Druid ()))
    makeBehavior f l s = do
      spinEvent <- EM.snap (onSelect s) (return $ s |@@| selection) 
      let behavior = eventCounter (clock 1) `switch` resettableClockCounter spinEvent
      return $ behavior |||-> (l, text)
    makeClock :: Double -> Druid (Event ())
    makeClock val = return $ traceEX "Tick" (clock val)
    eventCounter :: Event a -> Behavior (Druid Integer)
    eventCounter e = accum (return 0) (e -=> (liftM (1 +)))
    resettableClockCounter :: Event Int -> Event (Druid (Behavior (Druid Integer)))
    resettableClockCounter switch = switch ==> (\v -> makeClock (fromIntegral v :: Double) >>= \c -> return $ eventCounter c)

guiD :: Druid(Behavior (Druid ()))
guiD = do
  f <- createFrame [text := "Click", size := sz 400 400, on closing := exitSuccess]
  b <- createButton f [text := "Press", size := sz 30 30]
  randomButtonBehavior f b
  -- return $ lift0 empty
  where
    randomButtonBehavior :: Frame -> Button -> Druid (Behavior (Druid ()))
    randomButtonBehavior f b = do
      event <- onCommand b
      return $ hold empty (event -=> f ==> buildButtons)
      let ev = event -=> after (buildButtons f) (lift0 empty)
      return $ (lift0 empty) `switch` ev 
      -- let beh = (event -=> (liftM1 (seq $ buildButtons f))) beh
      -- lift0 empty `switch` beh
    buildButtons :: Frame -> Druid ()
    buildButtons f = do 
      (x1, y1) <- randomPoints
      (x2, y2) <- randomPoints
      b1 <- createButton f [text := "New-1", position := point x1 y1]
      b2 <- createButton f [text := "New-2", position := point x2 y2]
      return ()
    randomPoints :: Druid (Int, Int)
    randomPoints = do
      x <- liftIO $ randomRIO (0, 300)
      y <- liftIO $ randomRIO (0, 300)
      return $ (x, y)
    empty :: Druid ()
    empty = return ()
    after :: Druid () -> a -> Druid a
    after io v = do io
                    return v
    

main :: IO ()
main = do
  args <- getArgs
  case args of
    []    -> start $ startEngine guiA
    "a":_ -> start $ startEngine guiA
    "b":_ -> start $ startEngine guiAnimatingButton
    "c":_ -> start $ startEngine guiC
    "d":_ -> start $ startEngine guiD
    _     -> start $ startEngine guiA

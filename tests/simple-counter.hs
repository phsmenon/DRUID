{-# LANGUAGE ScopedTypeVariables  #-}
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

gui :: Druid(Behavior (Druid ()))
gui = do
  f <- createFrame [text := "Timing", on closing := exitSuccess]
  p <- createPanel f []
  l <- createLabel p [text := "Count"]
  i <- createLabel p [text := "Interval = 1"]
  s <- createSpin p 1 5 []
  setProperties f [size := sz 150 70]
  setProperties s [position := point 0 30]
  setProperties l [fontSize := 20]
  setProperties i [position := point 30 5]
  makeBehavior f l i s
  where
    makeBehavior :: Frame -> Label -> Label -> Spin -> Druid(Behavior (Druid ()))
    makeBehavior f l i s = do
      spinEvent <- EM.snap (onSelect s) (return $ s |@@| selection) 
      let intervalEvent :: (Event String) = spinEvent ==> (\v -> "Interval = " ++ show v)
      let intervalBehavior = lift0 "Interval = 1" `switch` (intervalSwitchEvent intervalEvent)
      let behavior = eventCounter (clock 1) `switch` resettableClockCounter spinEvent
      return $ (behavior |||-> (l, text)) $$ (intervalBehavior |-> (i, text))
    makeClock :: Double -> Druid (Event ())
    makeClock val = return $ traceEX "Tick" (clock val)
    eventCounter :: Event a -> Behavior (Druid Integer)
    eventCounter e = accum (return 0) (e -=> (liftM (1 +)))
    resettableClockCounter :: Event Int -> Event (Druid (Behavior (Druid Integer)))
    resettableClockCounter switch = switch ==> (\v -> makeClock (fromIntegral v :: Double) >>= \c -> return $ eventCounter c)
    intervalSwitchEvent :: Event String -> Event (Druid (Behavior String))
    intervalSwitchEvent event = event ==> (\v -> return $ lift0 v)

main :: IO ()
main = start $ startEngine gui

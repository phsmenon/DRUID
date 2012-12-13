module Main where

import Druid.DruidMonad
import Druid.Engine
import qualified Druid.EngineM as EM
import Druid.Controls

import Druid.ReactiveUI

import Graphics.UI.WX hiding (Button, Frame, Panel, Widget, Event)
import qualified Graphics.UI.WX as WX

import Control.Monad
import System.Exit
import System.Environment(getArgs)

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
  b <- createButton f [text := "Press Me"]
  setProperties f [size := sz 300 200]
  setProperties b [position := point 50 500, size := sz 80 10]
  makeBehavior f b
  where
    makeBehavior :: Frame -> Button -> Druid(Behavior (Druid ()))
    makeBehavior f b = do
      event <- onCommand b
      let x = (p2 50 50) + integral (p2 50 0)
      let beh = x |-> (b, position)
      return beh

{-guiB :: Druid(Behavior (Druid ()))-}
{-guiB = do-}
  {-f <- createFrame [text := "Timing", on closing := exitSuccess]-}
  {-l <- createLabel f [text := "Count"]-}
  {-s <- createSpin f 1 5 []-}
  {-setProperties f [size := sz 150 50]-}
  {-setProperties s [position := point 0 30]-}
  {-setProperties l [fontSize := 20]-}
  {-makeBehavior f l s-}
  {-where-}
    {-makeBehavior :: Frame -> Label -> Spin -> Druid(Behavior (Druid ()))-}
    {-makeBehavior f l s = do-}
      {-let baseBeh = accum (0::Integer) (clock 1 -=> (1 +))-}
      {-baseEvent <- onSelect s-}
      {-let spinVal = snap baseEvent $ s @@ text-}
      {-let iSpinVal = spinVal ==> toDouble-}
      {-let event = return $ makeClock iSpinVal-}
      {-newBeh <- EM.accum (return 0) (event EM.-=> return (1 +))-}
      {-let fullBeh = baseBeh `untilB` newBeh-}
      {-let beh' = lift1 show fullBeh |-> (l, text)-}
      {-return beh'-}
    {-toDouble :: Druid String -> Druid Double-}
    {-toDouble s = s >>= \str -> return $ (read str :: Double)-}
    {-makeClock :: Event (Druid Double) -> Event (Druid (Event ()))-}
    {-makeClock e = do-}
      {-e ==> (\v -> v >>= return . clock)-}


  
main :: IO ()
main = do
  args <- getArgs
  case args of
    []    -> start $ startEngine guiA
    "a":_ -> start $ startEngine guiA
    "b":_ -> start $ startEngine guiAnimatingButton
    _     -> start $ startEngine guiA

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
import System.Exit
import System.Environment(getArgs)
import Debug.Trace

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
      {-baseEvent <- onSelect s-}
      {-let baseBeh = makeAcc (clock 1)-}
      {-let newBeh = makeBeh baseEvent s-}
      {-let fullBeh = baseBeh `untilB` newBeh-}
      {-return $ fullBeh |||-> (l, text)-}
    {-toDouble :: Druid String -> Druid Double-}
    {-toDouble s = s >>= \str -> traceShow str $ return $ (read str :: Double)-}
    {-makeClock :: Druid Double -> Druid (Event ())-}
    {-makeClock = \v -> v >>= return . clock-}
    {-makeAcc :: Event a -> Behavior (Druid Integer)-}
    {-makeAcc e = accum (return 0) (e -=> (liftM (1 +)))-}
    {-makeBeh :: Event a -> Spin -> Event (Druid (Behavior (Druid Integer)))-}
    {-makeBeh switch s = switch -=> (makeClockFromSpin s >>= \c -> return makeAcc c)-}
    {-makeClockFromSpin :: Spin -> Druid (Event ())-}
    {-makeClockFromSpin s = let val = toDouble $ getProperty s text in makeClock val-}
                      



  
main :: IO ()
main = do
  args <- getArgs
  case args of
    []    -> start $ startEngine guiA
    "a":_ -> start $ startEngine guiA
    _     -> start $ startEngine guiA

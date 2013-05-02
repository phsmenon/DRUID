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

main :: IO ()
main = start $ startEngine guiA

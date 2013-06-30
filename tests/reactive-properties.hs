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
import Text.Printf
import Debug.Trace
import Data.List
import System.Exit
import System.Environment(getArgs)

import System.Random

cycleBehavior :: Show b => Event a -> [b] -> Behavior b
cycleBehavior ev lst = lift0 (head lst) `untilB` (ev -=> (return $ cycleBehavior ev $ cycleList lst))
  where
    cycleList lst = (tail lst) ++ [head lst]

guiA :: Druid(Behavior (Druid ()))
guiA = do
  f <- createFrame [text := "Reactive Properties", on closing := exitSuccess]
  l <- createLabel f [text := "0", clientSize := sz 75 150, position := point 30 20, fontSize := 20]
  b <- createButton f [text := "Press Me", position := point 100 20]
  setProperties f [outerSize := sz 500 300]
  makeBehavior f l b
  where
    makeBehavior :: Frame -> Label -> Button -> Druid(Behavior (Druid ()))
    makeBehavior f l b = do
      event <- onCommand b
      let beh = cycleBehavior event ["Bat", "Cat", "Dog", "Eel", "Frog"]
      let beh' = setText l beh
      let beh'' = setTextColor l (cycleBehavior (clock 2) [WX.red, WX.blue, WX.green])
      return $ (beh' $$ beh'')
main :: IO ()
main = start $ startEngine guiA

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
  f <- createFrame [text := "Reactive Properties", outerSize := sz 500 300, on closing := exitSuccess]
  l <- createLabel f [text := "0", clientSize := sz 200 150, position := point 30 20, fontSize := 16]
  b <- createButton f [text := "Press Me", position := point 200 20]
  r <- createRectangle f [outerSize := sz 75 75, position := point 100 20]
  c <- createEllipse f [outerSize := sz 75 75, position := point 100 125]
  makeBehavior f l r c b
  where
    makeBehavior :: Frame -> Label -> Rectangle -> Ellipse -> Button -> Druid(Behavior (Druid ()))
    makeBehavior f l r c b = do
      event <- onCommand b
      let beh = cycleBehavior event [WX.red, WX.blue, WX.green]
      let beh' = setBackground r beh
      let tick = clock 1
      let beh'' = setColor c (cycleBehavior tick [WX.red, WX.blue, WX.green])
      let beh''' = setText l (cycleBehavior tick ["Red", "Blue", "Green"])
      let beh'''' = setTextColor l (cycleBehavior tick [WX.red, WX.blue, WX.green])
      return $ beh' $$ beh'' $$ beh''' $$ beh''''

main :: IO ()
main = start $ startEngine guiA

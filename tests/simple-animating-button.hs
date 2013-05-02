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

-- Animating button
gui :: Druid(Behavior (Druid ()))
gui = do
  f <- createFrame [text := "Escaping Button", on closing := exitSuccess]
  p <- createPanel f []
  b <- createButton p [text := "Help Me"]
  setProperties f [clientSize := sz 500 200]
  setProperties b [position := pt 50 500, size := sz 100 50]
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

main :: IO ()
main = start $ startEngine gui

module Main where

import Control.Monad
import System.Exit


import Graphics.UI.WX
import Druid.WXExtensions

gui :: IO ()
gui = do
  f <- createFrame [text := "Click Count", on closing := exitSuccess]
  g1 <- createRectangle f [position := pt 0 0, outerSize := sz 100 100]
  g2 <- createEllipse f [position := pt 125 0, outerSize := sz 100 100]
  set f [size := sz 300 300]
  set g2 [color := rgb 255 0 0, bgcolor := rgb 0 255 255]

main :: IO ()
main = start  gui

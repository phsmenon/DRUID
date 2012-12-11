module Main where

import Druid.DruidMonad
import Druid.Engine
import Druid.ReactiveUI
import Druid.Controls

import Graphics.UI.WX

initialGui :: Druid ()
initialGui = do
  f <- createFrame "Foo Bar"
  l <- createLabel f "Text"
  b <- createButton f "Press Me"
  setButtonProperty b [position := point 50 50]
  

main :: IO ()
main = do
  --let initialBehavior = lift1 (setText l) $ accum "0" (onCommand b ==> (\_ -> strInc))
  --start $ startEngine (Just initialGui) (hold never)
  putStrLn "In Main"

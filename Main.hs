module Main where

import Druid.DruidMonad
import Druid.Engine
import Druid.ReactiveUI
import Druid.Controls

import qualified Graphics.UI.WX as WX  hiding ((:=))
import Graphics.UI.WX(Prop((:=)))

initialGui :: Druid(Behavior (Druid ()))
initialGui = do
  f <- createFrame "Foo Bar"
  l <- createLabel f "Text"
  b <- createButton f "Press Me"
  setButtonProperty b [WX.position := WX.point 50 50]
  fn l b 
  where
    fn :: Label -> Button -> Druid(Behavior (Druid ()))
    fn l b = do event <- onCommand b
                let beh = accum "" (event -=> ("a" ++))
                return $ lift1 (\val -> propSet l val) beh
    propSet :: Label -> String -> Druid ()
    propSet l str = setLabelProperty l [WX.text := str]
  

main :: IO ()
main = do
  --let initialBehavior = lift1 (setText l) $ accum "0" (onCommand b ==> (\_ -> strInc))
  --start $ startEngine (Just initialGui) (hold never)
  -- putStrLn "In Main"
  WX.start $ startEngine initialGui

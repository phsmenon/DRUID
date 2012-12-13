module Main where

import Druid.DruidMonad
import Druid.EngineM
import Druid.Controls

import qualified Druid.ReactiveUI as RUI

import qualified Graphics.UI.WX as WX  hiding ((:=))
import Graphics.UI.WX(Prop((:=)))

import Control.Monad
import System.Exit

showB :: Show a => Druid a -> Druid String
showB = liftM show


initialGui :: Druid(Behavior (Druid ()))
initialGui = do
  f <- createFrame [WX.text := "Foo Bar", WX.on WX.closing := exitSuccess]
  l <- createLabel f [WX.text := "Text"]
  b1 <- createButton f [WX.text := "Press Me"]
  setProperties b1 [WX.position := WX.point 50 50]
  fn f l b1
  where
    fn :: Frame -> Label -> Button -> Druid(Behavior (Druid ()))
    fn f l b = do beh <- accum (return 0) (RUI.onCommand b -=> return (10 +)) 
                  -- lift1 showB beh -- |-> return (l, WX.text)
                  -- lift0 (setProperty l (WX.text := "Foo"))
                  {-lift1 (return (\val -> propSet l (show val))) (return beh)-}
                  lift1 (return (\val -> setProperty l (WX.text := show val))) (return beh)
    {-fn f l b = do event <- onCommand b-}
                  {-let beh = accum (0::Integer) (event -=> (25 +))-}
                {--- return $ lift1 (\val -> propSet l val) beh-}
                  {-return $ lift1 (\val -> newLabel f val val (show val)) beh-}
    newLabel :: Frame -> Integer -> Integer -> String -> Druid ()
    newLabel f x y txt = do 
      l <- createLabel f [WX.text := txt]
      setProperties l [WX.position := WX.point (fromIntegral x) (fromIntegral y)]
    propSet :: Label -> String -> Druid ()
    propSet l str = setProperties l [WX.text := str]
  

main :: IO ()
main = do
  --let initialBehavior = lift1 (setText l) $ accum "0" (onCommand b ==> (\_ -> strInc))
  --start $ startEngine (Just initialGui) (hold never)
  -- putStrLn "In Main"
  WX.start $ startEngine initialGui

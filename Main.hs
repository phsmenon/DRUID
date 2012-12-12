module Main where

import Druid.DruidMonad
import qualified Druid.Engine as E
import Druid.ReactiveUI
import Druid.Controls

import qualified Graphics.UI.WX as WX  hiding ((:=))
import Graphics.UI.WX(Prop((:=)))

import Control.Monad


(-=>) = liftM2 (E.-=>)

lift1 = liftM2 E.lift1

accum = liftM2 E.accum


initialGui :: Druid(Behavior (Druid ()))
initialGui = do
  f <- createFrame "Foo Bar"
  l <- createLabel f "Text"
  b <- createButton f "Press Me"
  setButtonProperty b [WX.position := WX.point 50 50]
  fn f l b 
  where
    fn :: Frame -> Label -> Button -> Druid(Behavior (Druid ()))
    fn f l b = do beh <- accum (return 0) (onCommand b -=> return (10 +))
                  lift1 (return (\val -> propSet l (show val))) (return beh)
    {-fn f l b = do event <- onCommand b-}
                  {-let beh = accum (0::Integer) (event -=> (25 +))-}
                {--- return $ lift1 (\val -> propSet l val) beh-}
                  {-return $ lift1 (\val -> newLabel f val val (show val)) beh-}
    newLabel :: Frame -> Integer -> Integer -> String -> Druid ()
    newLabel f x y txt = do 
      l <- createLabel f txt
      setLabelProperty l [WX.position := WX.point (fromIntegral x) (fromIntegral y)]
    propSet :: Label -> String -> Druid ()
    propSet l str = setLabelProperty l [WX.text := str]
  

main :: IO ()
main = do
  --let initialBehavior = lift1 (setText l) $ accum "0" (onCommand b ==> (\_ -> strInc))
  --start $ startEngine (Just initialGui) (hold never)
  -- putStrLn "In Main"
  WX.start $ E.startEngine initialGui

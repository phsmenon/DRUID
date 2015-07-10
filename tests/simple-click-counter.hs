module Main where

import Druid.WX.Frp

import Control.Monad.IO.Class

import Graphics.UI.WX hiding (Button, Frame, Panel, Widget, Event)
import qualified Graphics.UI.WX as WX

-- Simple counter created by attaching a (guarded) behavior to a label's text property

gui :: Druid ()
gui = do 
  f <- createWXFrame [size :=~ sz 400 200]
  l <- createWXLabel f [fontSize :=~ 20, position :=~ point 200 50]
  b <- createWXButton f [position :=~ point 50 50, text :=~ "Click"]
  beh <- onCommand b >>= \ev -> return $ show `lift1` ( accum (0::Integer) (ev -=> (1 +)) )
  setAttr l text $ beh 

main :: IO ()
main = start $ startEngine gui

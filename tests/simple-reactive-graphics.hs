{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, ConstraintKinds #-}

module Main where

import Druid.WX.Frp

import Graphics.UI.WX hiding (Event, when)
import Graphics.UI.WXCore hiding (when)

import Text.Printf
import Text.Read
import Data.IORef
import Data.Maybe
import Control.Monad hiding (when)
import Control.Applicative
import Control.Monad.IO.Class
import System.Exit
import Debug.Trace


gui :: Druid ()
gui = do
  f <- createWXFrame [text :=~ "Reactive Properties", outerSize :=~ sz 450 350, on closing :=~ exitSuccess]
  l1 <- createWXLabel f [text :=~ "Click Section", position :=~ point 20 20]
  r <- createWXRectangle f [outerSize :=~ sz 75 75, position :=~ point 20 50]
  b <- createWXButton f [text :=~ "Press", position :=~ point 20 140]
  l1 <- createWXLabel f [text :=~ "Timer Section", position :=~ point 150 20]
  e <- createWXEllipse f [outerSize :=~ sz 75 75, position :=~ point 150 50]
  t <- createWXTimer f [interval :=~ 1000]
  lb <- createWXListBox f [outerSize :=~ sz 125 150, position :=~ point 280 20]
  tr <- createWXTextEntry f [outerSize :=~ sz 30 30, position :=~ point 280 180]
  tb <- createWXTextEntry f [outerSize :=~ sz 30 30, position :=~ point 320 180]
  tg <- createWXTextEntry f [outerSize :=~ sz 30 30, position :=~ point 360 180]
  lbadd <- createWXButton f [text :=~ "Add", position :=~ point 280 220]
  lbdel <- createWXButton f [text :=~ "Del", position :=~ point 280 250]
  lbApply <- createWXButton f [text :=~ "Apply", position :=~ point 280 280]
  buttonCommand <- onCommand b
  timerCommand <- onCommand t
  return buttonCommand >>= return . cycleBehavior [red, blue, green] >>= setAttr r bgcolor 
  return timerCommand >>= return . cycleBehavior [red, blue, green] >>= setAttr e bgcolor 
  onCommand lbadd >>= snapColor tr tb tg >>= flip (react lb) addColorMaybe
  onCommand lbApply >>= updateCycleColorsFor lb buttonCommand r
  onCommand lbApply >>= updateCycleColorsFor lb timerCommand e
  -- onCommand lbdel >>= whenValidSelection lb >>= snapIndex lb >>= flip (react lb) removeEntry
  return ()
  where
    cycleBehavior lst ev = {- traceB "Color: " $ -} lift1 head $ accum lst (ev -=> cycleList)
    cycleList lst = (tail lst) ++ [head lst]
    snapColor a b c ev = colorFromFields a b c >>= return . (snap ev)
    snapIndex lbox ev = getAttr lbox selection >>= return . (snap ev) 
    snapList lbox ev = getAttr lbox items >>= return . (snap ev)
    colorFromFields a b c = lift3 maybeColor <$> textInt a <*> textInt b <*> textInt c
    addColorMaybe lbox maybeClr = addEntry lbox $ map colorToString (maybeToList maybeClr)
    addEntry lbox lst = now lbox items >>= return . lift0 . (++ lst) >>= setAttr lbox items
    removeEntry lbox index = now lbox items >>= return . lift0 . (delIndex index) >>= setAttr lbox items
    colorToString :: Color -> String
    colorToString clr = printf "Color: %d %d %d" (colorRed clr :: Int)  (colorGreen clr :: Int) (colorBlue clr :: Int)
    stringToColor txt = let lst = words txt in rgb (read $ lst !! 1 :: Int) (read $ lst !! 2) (read $ lst !! 3)
    itemPlus lbox v = modifyAttr lbox
    asInt txt = readMaybe txt :: Maybe Int
    textInt w = getAttr w text >>= return . lift1 asInt
    maybeColor r g b = rgb <$> r <*> g <*> b
    delIndex i lst = let (xs, ys) = splitAt i lst in xs ++ (tail ys)
    updateCycleColors srcEv w lst = return (cycleBehavior lst srcEv) >>= setAttr w bgcolor
    updateCycleColorsFor lbox srcEv w ev = snapList lbox ev >>= return . (==> map stringToColor) >>= flip (react w) (updateCycleColors srcEv)
    -- whenValidSelection lbox ev = getAttr lbox selection >>= return . when . lift1 (/= -1) >>= return . (ev -=>)

main :: IO ()
main = start $ startEngine gui

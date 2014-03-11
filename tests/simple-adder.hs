{-# LANGUAGE ViewPatterns  #-}

module Main where

import Druid.DruidMonad
import Druid.Engine
import Druid.Controls
import Druid.ReactiveUI

import Data.Maybe
import Text.Read
import Control.Monad.IO.Class
import Debug.Trace

import Graphics.UI.WX hiding (Event)

-- Simple adder - Mixes behaviors and reactors

gui :: Druid ()
gui = do 
  f <- createWXFrame 
  a <- createWXTextEntry f 
  b <- createWXTextEntry f 
  l <- createWXLabel f 
  desc <- createWXLabel f 
  atext <- getAttr a text
  btext <- getAttr b text
  setAttrs f [text :=~ "Arithmetic", size :=~ sz 230 140]
  setAttrs desc [position :=~ point 10 10, text :=~ "Enter numbers in the text fields to add them"]
  setAttrs a [position :=~ point 10 30, size :=~ sz 50 25]
  setAttrs b [position :=~ point 70 30, size :=~ sz 50 25]
  setAttrs l [position :=~ point 130 30, size :=~ sz 40 30, fontSize :=~ 14, text :=: lift2 addNumbers atext btext]
  onTextChange a >>= \ev -> react a ev checkInteger
  onTextChange b >>= \ev -> react b ev checkInteger
  where
  asInteger txt = readMaybe txt :: Maybe Int
  checkInteger w _ = do
    t <- now w text 
    let clr = if t == "" || isJust (asInteger t) then white else red
    setAttr w bgcolor $ lift0 clr
  addNumbers (asInteger -> Just v1) (asInteger -> Just v2) = show $ v1 + v2
  addNumbers _ _ = "Error"

    

main :: IO ()
main = start $ startEngine gui


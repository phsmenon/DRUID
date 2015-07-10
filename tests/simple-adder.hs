{-# LANGUAGE ViewPatterns  #-}

module Main where

import Druid.WX.Frp

import Data.Maybe
import Text.Read
import Control.Monad.IO.Class
import Debug.Trace

import Graphics.UI.WX hiding (Event)

-- Simple adder - Mixes behaviors and reactors

gui :: Druid ()
gui = do 
  f <- createWXFrame [text :=~ "Arithmetic"]
  a <- createWXTextEntry f []
  b <- createWXTextEntry f []
  l <- createWXLabel f []
  desc <- createWXLabel f [ text :=~ "Enter numbers in the text fields to add them"]
  atext <- getAttr a text
  btext <- getAttr b text
  setAttrs l [text :=: lift2 addNumbers atext btext]
  let ly = column 3 [getLayout desc, 
                     row 3 [rigid $ getLayout a, rigid $ getLayout b, rigid $ getLayout l]]
  setAttrs f [layout :=~ margin 20 ly]
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


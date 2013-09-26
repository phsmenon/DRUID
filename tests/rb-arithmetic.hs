{-# LANGUAGE ScopedTypeVariables  #-}
module Main where

import Druid.DruidMonad
import Druid.Engine
import qualified Druid.EngineM as EM
import Druid.Controls

import Druid.ReactiveUI

import Graphics.UI.WX hiding (Button, Frame, Panel, Widget, Event)
import qualified Graphics.UI.WX as WX

import Control.Monad
import Control.Applicative
import System.Exit

import System.Random

gui :: Druid(Behavior (Druid ()))
gui = do
  f  <- createFrame [text := "Arithmetic", on closing := exitSuccess]
  a <- createTextField f [ text := "0"]
  b <- createTextField f [ text := "0"]
  l <- createLabel f [ text := "0"]
  setProperties f [size := sz 300 200]
  setProperties a [position := point 0 20, size := sz 50 20]
  setProperties b [position := point 60 20, size := sz 50 20]
  setProperties l [position := point 120 20, size := sz 30 20]
  makeBehavior f a b l
  where
    makeBehavior :: Frame -> TextField -> TextField -> Label -> Druid(Behavior (Druid ()))
    makeBehavior f a b l = do
      return $ setText l (getText a)      

main :: IO ()
main = start $ startEngine gui

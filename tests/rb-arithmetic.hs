{-# LANGUAGE ScopedTypeVariables  #-}
module Main where

import Druid.DruidMonad
import Druid.Engine
import qualified Druid.EngineM as EM
import Druid.Controls

import Druid.ReactiveUI

import Graphics.UI.WX hiding (Button, Frame, Panel, Widget, Event)
import qualified Graphics.UI.WX as WX
import qualified Graphics.UI.WXCore as WXC

import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import System.Exit

gui :: Druid(Behavior (Druid ()))
gui = do
  f <- createFrame [text := "Arithmetic", size := sz 200 140, on closing := exitSuccess]
  a <- createTextField f [position := point 10 30, size := sz 50 30, text := "0"]
  b <- createTextField f [position := point 70 30, size := sz 50 30, text := "0"]
  l <- createLabel f [position := point 130 30, size := sz 40 30, fontSize := 14, text := "0"]
  {-ffnt <- liftIO $ WXC.fontCreate 8 0 0 0 False "" WXC.wxFONTENCODING_SYSTEM-}
  {-getDelegate a >>= \w -> liftIO $ WXC.windowSetFont w ffnt-}
  makeBehavior a b l
  where
    makeBehavior :: TextField -> TextField -> Label -> Druid(Behavior (Druid ()))
    makeBehavior a b l = do
      let (data1, data2) = (integerTextBehavior a, integerTextBehavior b)
      let result = lift2 (liftM2 (+)) data1 data2
      let rtext = lift1 ( maybe "ERR" show ) result
      return $ setText l rtext 
    integerTextBehavior :: TextField -> Behavior (Maybe Integer)
    integerTextBehavior t = do
      let behVal = lift1 maybeRead $ getText t
      let fnColor = maybe (setProperty t $ WX.bgcolor := WX.red) (const $ setProperty t $ WX.bgcolor := WX.white)
      liftM1 (\maybeV -> fnColor maybeV >> return maybeV) behVal
    maybeRead :: Read a => String -> Maybe a
    maybeRead = fmap fst . listToMaybe . reads     

main :: IO ()
main = start $ startEngine gui

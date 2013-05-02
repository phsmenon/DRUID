module Main where

import Druid.DruidMonad
import Druid.Engine
import qualified Druid.EngineM as EM
import Druid.Controls

import Druid.ReactiveUI

import Graphics.UI.WX hiding (Button, Frame, Panel, Widget, Event)
import qualified Graphics.UI.WX as WX

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import System.Exit
import System.Environment(getArgs)

import System.Random

gui :: Druid(Behavior (Druid ()))
gui = do
  f <- createFrame [text := "Click", size := sz 400 400, on closing := exitSuccess]
  p <- createPanel f []
  b <- createButton p [text := "Press", size := sz 70 30]
  randomButtonBehavior p b
  -- return $ lift0 empty
  where
    randomButtonBehavior :: Panel -> Button -> Druid (Behavior (Druid ()))
    randomButtonBehavior p b = do
      event <- onCommand b
      -- return $ hold empty (event -=> p ==> buildButtons)
      -- let ev = event -=> after (buildButtons p) (lift0 empty)
      -- return $ (lift0 empty) `switch` ev 
      -- let beh = (event -=> (liftM1 (seq $ buildButtons f))) beh
      -- lift0 empty `switch` beh
      let behavior = lift0 empty `switchLater` (event -=> buildButtons p)
      return behavior
    buildButtons :: Panel -> Druid (Behavior (Druid ()))
    buildButtons p = do 
      (x1, y1) <- randomPoints
      (x2, y2) <- randomPoints
      b1 <- createButton p [text := "New-1", position := point x1 y1]
      b2 <- createButton p [text := "New-2", position := point x2 y2]
      -- return ()
      beh1 <- (randomButtonBehavior p b1)
      beh2 <- (randomButtonBehavior p b2)
      return (beh1 $$ beh2)
    randomPoints :: Druid (Int, Int)
    randomPoints = do
      x <- liftIO $ randomRIO (0, 300)
      y <- liftIO $ randomRIO (0, 300)
      return $ (x, y)
    empty :: Druid ()
    empty = return ()
    after :: Druid () -> a -> Druid a
    after io v = do io
                    return v
    

main :: IO ()
main = start $ startEngine gui

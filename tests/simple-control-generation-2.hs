module Main where

import Druid.WX.Frp

import Data.Char
import Control.Applicative
import Control.Monad.IO.Class
import System.Random
import Debug.Trace

import Graphics.UI.WX

-- Simple example of attaching a reactor to a button to (recursively) create other buttons
-- Without reactors, this would have to be a carefully written switch

gui :: Druid ()
gui = do
  f <- createWXFrame [size :=~ sz 800 800]
  l <- createWXLabel f [text :=~ "Generate new buttons by clicking on *any* button"]
  b <- createWXButton f [position :=~ point 0 25, size :=~ sz 100 50, text :=~ "A"]
  onCommand b >>= \ev -> react b ev (generateRandomButton f)
  where
    generateRandomButton :: WXFrame -> WXButton -> a -> Druid ()
    generateRandomButton f source _ = do 
      p <- randomPair 100 700 >>= \(x,y) -> return $ point x y
      s <- liftIO $ randomRIO (50, 100) >>= \x -> return $ sz x 40
      c <- liftIO $ randomRIO (0, 2) >>= \i -> return ( [red, green, blue] !! i )
      t <- liftIO $ randomRIO (0, 10) :: (Druid Int)
      b <- createWXButton f []
      stext <- getAttr source text
      setAttrs b [position :=~ p, size :=~ s, bgcolor :=~ c, text :=: (lift1 appendNext stext) ]
      onCommand b >>= \ev -> react b ev (generateRandomButton f)
    randomPair :: Int -> Int -> Druid (Int, Int)
    randomPair a b = liftIO $ randomRIO (a, b) >>= \x -> randomRIO (a, b) >>= \y -> return (x, y)
    appendNext :: String -> String
    appendNext text = 
      let next = ord (last text) + 1 in
      text ++ [if next > ord 'Z' then 'A' else chr next]
      

main :: IO ()
main = start $ startEngine gui



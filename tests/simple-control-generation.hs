module Main where

import Druid.WX.Frp

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import System.Random
import Debug.Trace

import Graphics.UI.WX

-- Simple example of using a reactor to do things when specific events occur

gui :: Druid ()
gui = do
  f <- createWXFrame [size :=~ sz 800 800]
  l <- createWXLabel f [text :=~ "Generate new labels by clicking on the button"]
  b <- createWXButton f [position :=~ point 0 25, size :=~ sz 100 50, text :=~ "Generate"]
  onCommand b >>= \ev -> react b ev (generateRandomLabel f)
  where
    generateRandomLabel :: ReactiveProxy rp => WXFrame -> rp -> a -> Druid ()
    generateRandomLabel f _ _ = do 
      p <- randomPair 100 700 >>= \(x,y) -> return $ point x y
      c <- liftIO $ randomRIO (0, 2) >>= \i -> return ( [red, green, blue] !! i )
      t <- liftIO $ randomRIO (0, 10) :: (Druid Int)
      l <- createWXLabel f []
      setAttrs l [position :=~ p, size :=~ sz 40 30, bgcolor :=~ c, text :=~ (" " ++ show t ++ " ")]
      setAttrs l [color :=~ white, fontSize :=~ 15, border :=~ BorderSimple]
      -- join $ react <$> (return l) <*> (onClickRight l) <*> (return (\_ _ -> remove l))
      onClickRight l >>= \e -> react l e (\_ _ -> remove l)
    randomPair :: Int -> Int -> Druid (Int, Int)
    randomPair a b = liftIO $ randomRIO (a, b) >>= \x -> randomRIO (a, b) >>= \y -> return (x, y)
      

main :: IO ()
main = start $ startEngine gui


module Druid.Types where

--import Graphics.UI.WX

------------------------------------------------------------------
-- Data Types for the Engine
------------------------------------------------------------------

data UIEvent = Initialize

type Stimulus = (Double, Maybe UIEvent)

data Behavior a = Behavior (Stimulus -> IO (Behavior a, a))

data Event a = Event (Stimulus -> IO (Event a, Maybe a))



class Num a => Vec a where
  (*^) :: Double -> a -> a

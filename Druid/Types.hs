module Druid.Types where

import Druid.DruidMonad

------------------------------------------------------------------
-- Data Types for the Engine
------------------------------------------------------------------

type Stimulus = (Double, Maybe UIEvent)

data Behavior a = Behavior (Stimulus -> Druid (Behavior a, a))

data Event a = Event (Stimulus -> Druid (Event a, Maybe a))


class Num a => Vec a where
  (*^) :: Double -> a -> a

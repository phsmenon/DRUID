module Druid.ReactiveUI where

import Druid.Engine
import Druid.Types
import Druid.DruidMonad

onCommand :: Integer -> Event ()
onCommand id = stimEvent f where
  f e | e == (Command id) = Just ()
  f _ = Nothing

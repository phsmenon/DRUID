{-# LANGUAGE FlexibleContexts, ExistentialQuantification #-}

module Druid.ReactiveUI where

import Druid.Engine
import Druid.DruidMonad
import Druid.Controls

import Data.Typeable
import Control.Arrow
import Control.Monad

import qualified Graphics.UI.WX as WX  hiding ((:=))
import Graphics.UI.WX(Prop((:=)))

-----------------------------------------------------------------------------

data AttributeBehavior w
  = forall a. (Typeable a, ReactiveProxy w) => (:=:) (Attribute w a) (Behavior a)
  | forall a. (Typeable a, ReactiveProxy w) => (:=~) (Attribute w a) a

setAttrs :: ReactiveProxy w => w -> [AttributeBehavior w] -> Druid ()
setAttrs w av = mapM_ set av where
  set (a :=: bv) = setAttr w a bv
  set (a :=~ v)  = setAttr w a $ lift0 v

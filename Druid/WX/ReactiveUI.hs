{-# LANGUAGE FlexibleContexts, ExistentialQuantification, ConstraintKinds, GADTs, GADTSyntax #-}

module Druid.WX.ReactiveUI where

import Druid.Engine
import Druid.WX.DruidMonad

-----------------------------------------------------------------------------

data AttributeBehavior w where
  (:=:) :: (ReactiveProxy w, AttributeC w a) => (Attribute w a) -> (Behavior a) -> AttributeBehavior w
  (:=~) :: (ReactiveProxy w, AttributeC w a) => (Attribute w a) -> a -> AttributeBehavior w


setAttrs :: (ReactiveProxy w, Monad (ReactorMonad w)) => w -> [AttributeBehavior w] -> ReactorMonad w ()
setAttrs w av = mapM_ (setAttrB w) av where
  setAttrB :: ReactiveProxy w => w -> AttributeBehavior w -> ReactorMonad w ()
  setAttrB w (a :=: bv) = setAttr w a bv
  setAttrB w (a :=~ v)  = setAttr w a $ lift0 v

modifyAttr :: (ReactiveProxy w, Monad (ReactorMonad w), AttributeC w a) => w -> Attribute w a -> (Behavior a -> Behavior a) -> ReactorMonad w ()
modifyAttr w attr fn = getAttr w attr >>= return . fn >>= setAttr w attr

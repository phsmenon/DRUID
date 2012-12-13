{-# LANGUAGE NoMonomorphismRestriction #-}

module Druid.EngineM where

import Druid.Types
import Druid.DruidMonad
import qualified Druid.Engine as E

import Control.Monad

----------------------------------------------------------------
-- Monadic lifted operators
----------------------------------------------------------------

lift0 = liftM E.lift0
lift1 = liftM2 E.lift1
lift2 = liftM3 E.lift2
lift3 = liftM4 E.lift3

(-=>) = liftM2 (E.-=>)
(==>) = liftM2 (E.==>)
(.|.) = liftM2 (E..|.)

choose = liftM3 E.choose
accum = liftM2 E.accum

integral :: E.Vec a => Druid (Behavior a) -> Druid (Behavior a)
integral = liftM E.integral

never = return E.never
once = liftM E.once
when = liftM E.when

hold = liftM2 E.hold
snap = liftM2 E.snap

switch = liftM2 E.switch
untilB = liftM2 E.untilB

startEngine = E.startEngine

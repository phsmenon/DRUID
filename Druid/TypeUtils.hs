{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts  #-}
module Druid.TypeUtils where

import Druid.Engine
import Druid.WX.DruidMonad

-----------------------------------------------------------
-- Helpers for Numeric Behaviors
-----------------------------------------------------------

-- Make (most) methods in Num reactive
instance (Monad m, Num a) => Num (SignalC m a) where
  (+)           = lift2 (+)
  (-)           = lift2 (-)
  (*)           = lift2 (*)
  abs           = lift1 abs
  negate        = lift1 negate
  fromInteger i = lift0 (fromInteger i)
  signum        = error "Cant use signum on behaviors"

-- Make (most) methods in Fractional reactive
instance (Fractional a, Monad m) => Fractional (SignalC m a)
  where
    (/) = lift2 (/)
    fromRational r = lift0 (fromRational r)



-----------------------------------------------------------
-- Helpers for Boolean Behaviors
-----------------------------------------------------------

-- Make reactive versions of && and ||
(&&*), (||*) :: Monad m => SignalC m Bool -> SignalC m Bool -> SignalC m Bool
(&&*) = lift2 (&&)
(||*) = lift2 (||)

-----------------------------------------------------------
-- Helpers for String Behaviors
-----------------------------------------------------------

(+++) :: Monad m => SignalC m String -> SignalC m String -> SignalC m String
(+++) = lift2 (++)


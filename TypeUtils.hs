module Druid.TypeUtils -- (
--  BD, BB, BS
--) 
where

import Druid.Types
import Druid.Engine

-----------------------------------------------------------
-- Abbrevations for common behavior types
-----------------------------------------------------------


-----------------------------------------------------------
-- Helpers for Numeric Behaviors
-----------------------------------------------------------

-- Make (most) methods in Num reactive
instance Num a => Num (Behavior a) where
  (+)           = lift2 (+)
  (-)           = lift2 (-)
  (*)           = lift2 (*)
  abs           = lift1 abs
  negate        = lift1 negate
  fromInteger i = lift0 (fromInteger i)
  signum        = error "Cant use signum on behaviors"

  
-- Make (most) methods in Fractional reactive
instance Fractional a => Fractional (Behavior a)
  where
    (/) = lift2 (/)
    fromRational r = lift0 (fromRational r)



-----------------------------------------------------------
-- Helpers for Boolean Behaviors
-----------------------------------------------------------

-- Make reactive versions of && and ||
(&&*), (||*) :: BB -> BB -> BB
(&&*) = lift2 (&&)
(||*) = lift2 (||)

-----------------------------------------------------------
-- Helpers for String Behaviors
-----------------------------------------------------------

(+++) :: BS -> BS -> BS
(+++) = lift2 (++)

-----------------------------------------------------------
-- Helper instances for Vec
-----------------------------------------------------------

instance Vec Integer where
  (*^) = (*)

instance Vec Double where
  (*^) = (*)


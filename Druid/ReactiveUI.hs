module Druid.ReactiveUI where

import Druid.Engine
import Druid.Types
import Druid.DruidMonad
import Druid.Controls

import qualified Graphics.UI.WX as WX  hiding ((:=))
import Graphics.UI.WX(Prop((:=)))

---------------------------------------------------------------------
-- Reactive UI combinators
---------------------------------------------------------------------

($$) :: Behavior (Druid ()) -> Behavior (Druid ()) -> Behavior (Druid ())
($$) = lift2 (>>)

(|->) :: Widget w => Behavior a -> (w, Attribute w a) -> Behavior (Druid ())
(|->) behavior (w, attr) = lift1 (\v -> setProperty w (attr := v)) behavior

(@@) :: Widget w => w -> Attribute w a -> Behavior (Druid a)
(@@) w attr = lift1 (uncurry getProperty) (lift0 (w, attr))

---------------------------------------------------------------------
-- Reactive UI specific Events
---------------------------------------------------------------------

onCommand :: CommandEventSource widget => widget -> Druid (Event ())
onCommand widget = do
  listener <- standardEventReceiver
  registerCommandListener widget listener
  return $ stimEvent f 
  where
    f (Command id) | id == (getId widget) = Just ()
    f _ = Nothing

onSelect :: SelectEventSource widget => widget -> Druid (Event ())
onSelect widget = do
  listener <- standardEventReceiver
  registerSelectListener widget listener
  return $ stimEvent f 
  where
    f (Select id) | id == (getId widget) = Just ()
    f _ = Nothing


{-propertyBehavior :: Control w => Behavior a -> w -> WX.Attr wx a -> Behavior (Druid ())-}
{-propertyBehavior behavior widget attribute = lift1 f behavior where-}
  {-f :: a -> Druid ()-}
  {-f val = do let id = getId w-}
             

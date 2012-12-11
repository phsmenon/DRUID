module Druid.ReactiveUI where

import Druid.Engine
import Druid.Types
import Druid.DruidMonad
import Druid.Controls

onCommand :: CommandEventSource widget => widget -> Druid (Event ())
onCommand widget = do
  listener <- standardEventReceiver
  registerCommandListener widget listener
  return $ stimEvent f 
  where
    f (Command id) | id == (getId widget) = Just ()
    f _ = Nothing


{-propertyBehavior :: Control w => Behavior a -> w -> WX.Attr wx a -> Behavior (Druid ())-}
{-propertyBehavior behavior widget attribute = lift1 f behavior where-}
  {-f :: a -> Druid ()-}
  {-f val = do let id = getId w-}
             

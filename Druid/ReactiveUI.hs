{-# LANGUAGE FlexibleContexts #-}

module Druid.ReactiveUI where

import Druid.Engine
import qualified Druid.Engine as EM
import Druid.Types
import Druid.DruidMonad
import Druid.Controls

import Control.Arrow
import Control.Monad

import qualified Graphics.UI.WX as WX  hiding ((:=))
import Graphics.UI.WX(Prop((:=)))

---------------------------------------------------------------------
-- Reactive UI combinators
---------------------------------------------------------------------

($$) :: Behavior (Druid ()) -> Behavior (Druid ()) -> Behavior (Druid ())
($$) = lift2 (>>)

(|->) :: Widget w => Behavior a -> (w, Attribute w a) -> Behavior (Druid ())
(|->) behavior (w, attr) = lift1 (\v -> setProperty w (attr := v)) behavior

(||->) :: (Widget w, Show a) => Behavior a -> (w, Attribute w String) -> Behavior (Druid ())
(||->) behavior (w, attr) = lift1 (\v -> setProperty w (attr := show v)) behavior

(|||->) :: (Widget w, Show a) => Behavior (Druid a) -> (w, Attribute w String) -> Behavior (Druid ())
(|||->) behavior (w, attr) = lift1 (\v -> v >>= \v -> setProperty w (attr := show v)) behavior

(@@) :: Widget w => w -> Attribute w a -> Behavior (Druid a)
(@@) w attr = lift1 (uncurry getProperty) (lift0 (w, attr))

(|@@|) :: Widget w => w -> Attribute w a -> Behavior a
(|@@|) w attr = liftM1 (uncurry getProperty) (lift0 (w,attr))

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


getSize :: (Widget w, WX.Dimensions (Delegate w)) => w -> Behavior (Integer, Integer)
getSize w = lift1 (width &&& height) $ w |@@| WX.outerSize where
  width sz = fromIntegral $ WX.sizeW sz
  height sz = fromIntegral$ WX.sizeH sz

setSize :: (Widget w, WX.Dimensions (Delegate w)) => w -> Behavior (Integer, Integer) -> Behavior (Druid ())
setSize w behSz = beh' |-> (w, WX.outerSize) where
  beh' = lift1 (\(wd, ht) -> WX.sz (fromIntegral wd) (fromIntegral ht)) behSz

getPosition :: (Widget w, WX.Dimensions (Delegate w)) => w -> Behavior (Integer, Integer)
getPosition w = lift1 (x &&& y) $ w |@@| WX.position where
  x pt = fromIntegral $ WX.pointX pt
  y pt = fromIntegral $ WX.pointY pt

setPosition :: (Widget w, WX.Dimensions (Delegate w)) => w -> Behavior (Integer, Integer) -> Behavior (Druid ())
setPosition w behPos = beh' |-> (w, WX.position) where
  beh' = lift1 (\(x, y) -> WX.point (fromIntegral x) (fromIntegral y)) behPos

getPreferredSize :: (Widget w, WX.Dimensions (Delegate w)) => w -> Behavior (Integer, Integer)
getPreferredSize w = lift1 (width &&& height) $ w |@@| WX.bestSize where
  width sz = fromIntegral $ WX.sizeW sz
  height sz = fromIntegral$ WX.sizeH sz


setFontFace :: (Widget w, WX.Literate (Delegate w)) => w -> Behavior String -> Behavior (Druid ())
setFontFace w behFace = behFace |-> (w, WX.fontFace)

setFontSize :: (Widget w, WX.Literate (Delegate w)) => w -> Behavior Integer -> Behavior (Druid ())
setFontSize w behSz = lift1 (\sz -> fromIntegral sz) behSz |-> (w, WX.fontSize)

setFontBold :: (Widget w, WX.Literate (Delegate w)) => w -> Behavior Bool -> Behavior (Druid ())
setFontBold w behBold = lift1 (\b -> if b then WX.WeightBold else WX.WeightNormal) behBold |-> (w, WX.fontWeight)

setFontItalic :: (Widget w, WX.Literate (Delegate w)) => w -> Behavior Bool -> Behavior (Druid ())
setFontItalic w behIt = lift1 (\b -> if b then WX.ShapeItalic else WX.ShapeNormal) behIt |-> (w, WX.fontShape)

setTextColor :: (Widget w, WX.Literate (Delegate w)) => w -> Behavior (WX.Color) -> Behavior (Druid ())
setTextColor w behColor = behColor |-> (w, WX.textColor)

setTextBackground :: (Widget w, WX.Literate (Delegate w)) => w -> Behavior (WX.Color) -> Behavior (Druid ())
setTextBackground w behColor = behColor |-> (w, WX.textBgcolor)

setColor :: (Widget w, WX.Colored (Delegate w)) => w -> Behavior (WX.Color) -> Behavior (Druid ())
setColor w behColor = behColor |-> (w, WX.color)

setBackground :: (Widget w, WX.Colored (Delegate w)) => w -> Behavior (WX.Color) -> Behavior (Druid ())
setBackground w behColor = behColor |-> (w, WX.bgcolor)


getText :: (Widget w, WX.Textual (Delegate w)) => w -> Behavior String
getText w = w |@@| WX.text

setText :: (Widget w, WX.Textual (Delegate w)) => w -> Behavior String -> Behavior (Druid ())
setText w behText = behText |-> (w, WX.text)


{-# LANGUAGE TypeFamilies, ExistentialQuantification, RankNTypes, ConstraintKinds #-}

module Druid.Controls where

import qualified Graphics.UI.WX as WX  hiding ((:=))
import Graphics.UI.WX(Prop((:=)), Attr(..))

import Control.Monad.IO.Class

import Druid.DruidMonad

---------------------------------------------------------------
-- Types Ahoy
---------------------------------------------------------------

class (Property w ~ Prop (Delegate w), Attribute w ~ Attr (Delegate w)) => Widget w where
  type Delegate w :: *
  type Attribute w :: * -> *
  type Property w :: *
  getId :: w -> Integer
  getDelegate :: w -> Druid (Delegate w)
  getProperty :: w -> Attribute w a -> Druid a
  setProperties :: w -> [Property w] -> Druid ()
  setProperty :: w -> Property w -> Druid ()
  setProperty w prop = setProperties w [prop]
  remove :: w -> Druid ()

class Widget w => Container w where
  getDelegateContainer :: w -> Druid(Delegate w)

class Widget w => CommandEventSource w where
  registerCommandListener :: w -> (UIEvent -> IO ()) -> Druid ()

class Widget w => SelectEventSource w where
  registerSelectListener :: w -> (UIEvent -> IO ()) -> Druid ()

---------------------------------------------------------------
-- Frame
---------------------------------------------------------------

data Frame = Frame Integer

instance Widget Frame where
  type Delegate Frame = WX.Frame ()
  type Attribute Frame = WX.Attr (WX.Frame ())
  type Property Frame = Prop (WX.Frame ())
  getId (Frame id) = id
  getDelegate (Frame id) = getWXWidget id >>= \(WXFrame w) -> return w
  getProperty = getWidgetProperty
  setProperties w props = addUpdateOp $ setWidgetProperties w props
  remove w = addRemoveOp $ setWidgetProperties w [WX.visible := False]

instance Container Frame where
  getDelegateContainer = getDelegate

createFrame :: [Property Frame] -> Druid Frame
createFrame props = do
  id <- getNextId 
  createTopLevelWidget id (WX.frame props) WXFrame
  return $ Frame id

---------------------------------------------------------------
-- Label
---------------------------------------------------------------

data Label = Label Integer

instance Widget Label where
  type Delegate Label = WX.StaticText ()
  type Attribute Label = WX.Attr (WX.StaticText ())
  type Property Label = Prop (WX.StaticText ())
  getId (Label id) = id
  getDelegate (Label id) = getWXWidget id >>= \(WXLabel w) -> return w
  getProperty = getWidgetProperty
  setProperties w props = addUpdateOp $ setWidgetProperties w props
  remove w = addRemoveOp $ setWidgetProperties w [WX.visible := False]


createLabel :: Container c => c -> [Property Label] -> Druid Label
createLabel parent props = do
  id <- getNextId
  createControlWidget id (getId parent) (\w -> WX.staticText w props) WXLabel
  return $ Label id

---------------------------------------------------------------
-- Button
---------------------------------------------------------------

data Button = Button Integer
  
instance Widget Button where
  type Delegate Button = WX.Button ()
  type Attribute Button = WX.Attr (WX.Button ())
  type Property Button = Prop (WX.Button ())
  getId (Button id) = id
  getDelegate (Button id) = getWXWidget id >>= \(WXButton w) -> return w
  getProperty = getWidgetProperty
  setProperties w props = addUpdateOp $ setWidgetProperties w props
  remove w = addRemoveOp $ setWidgetProperties w [WX.visible := False]

getButtonDelegate :: Integer -> Druid (Delegate Button)
getButtonDelegate id = getWXWidget id >>= \(WXButton w) -> return w


instance CommandEventSource Button where
  registerCommandListener (Button id) handler = 
    deferRegisterEventHandler id getButtonDelegate WX.command (handler $ Command id)

createButton :: Container c => c -> [Property Button] -> Druid Button
createButton parent props = do
  id <- getNextId
  createControlWidget id (getId parent) (\w -> WX.button w props) WXButton
  return $ Button id

---------------------------------------------------------------
-- Panel
---------------------------------------------------------------

data Panel = Panel Integer

instance Widget Panel where
  type Delegate Panel = WX.Panel ()
  type Attribute Panel = WX.Attr (WX.Panel ())
  type Property Panel = Prop (WX.Panel ())
  getId (Panel id) = id
  getDelegate (Panel id) = getWXWidget id >>= \(WXPanel w) -> return w
  getProperty = getWidgetProperty
  setProperties w props = addUpdateOp $ setWidgetProperties w props
  remove w = addRemoveOp $ setWidgetProperties w [WX.visible := False]

createPanel :: Container c => c -> [Property Panel] -> Druid Panel
createPanel parent props = do
  id <- getNextId
  createControlWidget id (getId parent) (\w -> WX.panel w props) WXPanel
  return $ Panel id
  
---------------------------------------------------------------
-- Spin Control
---------------------------------------------------------------

data Spin = Spin Integer

instance Widget Spin where
  type Delegate Spin = WX.SpinCtrl ()
  type Attribute Spin = WX.Attr (WX.SpinCtrl ())
  type Property Spin = Prop (WX.SpinCtrl ())
  getId (Spin id) = id
  getDelegate (Spin id) = getWXWidget id >>= \(WXSpin w) -> return w
  getProperty = getWidgetProperty
  setProperties w props = addUpdateOp $ setWidgetProperties w props
  remove w = addRemoveOp $ setWidgetProperties w [WX.visible := False]

createSpin :: Container c => c -> Integer -> Integer -> [Property Spin] -> Druid Spin
createSpin parent min max props = do
  id <- getNextId
  let (iMin, iMax) = (fromIntegral min, fromIntegral max)
  createControlWidget id (getId parent) (\w -> WX.spinCtrl w iMin iMax props) WXSpin
  return $ Spin id  

getSpinDelegate :: Integer -> Druid (Delegate Spin)
getSpinDelegate id = getWXWidget id >>= \(WXSpin w) -> return w


instance SelectEventSource Spin where
  registerSelectListener (Spin id) handler = 
    deferRegisterEventHandler id getSpinDelegate WX.select (handler $ Select id)

---------------------------------------------------------------
-- Internal Timer
---------------------------------------------------------------
  
createInternalTimer :: Integer -> (UIEvent -> IO ()) -> Druid ()
createInternalTimer time callback = do
  f <- liftIO $ WX.frame  [WX.text := "Internal Window for Timer", WX.visible := False]
  liftIO $ WX.timer f [WX.interval := (fromIntegral time), WX.on WX.command := callback Heartbeat]
  return ()
  
---------------------------------------------------------------
-- Helper Functions
---------------------------------------------------------------

deferRegisterEventHandler :: b -> (b -> Druid w) -> WX.Event w a -> a -> Druid ()
deferRegisterEventHandler id lookup event handler = do
  let action wxobj = WX.set wxobj [WX.on event := handler]
  addUpdateOp $ lookup id >>= \v -> liftIO $ action v

--deferRegisterEventHandler :: Widget w => w -> 
  
createTopLevelWidget :: Integer -> IO a -> (a -> WXWidget) -> Druid ()
createTopLevelWidget id delegate wrapper = addCreateOp $ do
  w <- liftIO delegate
  storeDelegate id (wrapper w)
  
createControlWidget :: Integer -> Integer -> (forall b. WX.Window b -> IO a) -> (a -> WXWidget) -> Druid ()
createControlWidget id parent delegate wrapper = addCreateOp $ do
  WXWindow w <- getWXWindow parent
  w <- liftIO $ delegate w
  storeDelegate id (wrapper w)  

setWidgetProperties :: Widget w => w -> [Property w] -> Druid ()
setWidgetProperties widget props = do
  wxObj <- getDelegate widget
  liftIO $ WX.set wxObj props

getWidgetProperty :: Widget w => w -> Attribute w a -> Druid a
getWidgetProperty widget attr = do
  wxObj <- getDelegate widget
  liftIO $ WX.get wxObj attr

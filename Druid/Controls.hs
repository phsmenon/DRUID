{-# LANGUAGE TypeFamilies, ExistentialQuantification, RankNTypes, ConstraintKinds, FlexibleContexts, UndecidableInstances #-}

module Druid.Controls where

import qualified Graphics.UI.WX as WX  hiding ((:=))
import Graphics.UI.WX(Prop((:=)), Attr(..))

import qualified Graphics.UI.WXCore as WXCore

import qualified Druid.WXExtensions as WXExt

import Control.Monad.IO.Class

import Druid.DruidMonad

import Debug.Trace

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

class (Widget w, WXExt.GraphicsContainer (GraphicsContainerDelegate w)) => Container w where
  type GraphicsContainerDelegate w :: *
  getContainerDelegate :: w -> Druid (Delegate w)
  getGraphicsContainer :: w -> Druid (GraphicsContainerDelegate w)

  getContainerDelegate = getDelegate

class Widget w => CommandEventSource w where
  registerCommandListener :: w -> (UIEvent -> IO ()) -> Druid ()

class Widget w => SelectEventSource w where
  registerSelectListener :: w -> (UIEvent -> IO ()) -> Druid ()

class Widget w => ResizeEventSource w where
  registerResizeListener :: w -> (UIEvent -> IO ()) -> Druid ()

class Widget w => TextChangeEventSource w where
  registerTextChangeListener :: w -> (UIEvent -> IO ()) -> Druid ()

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
  type GraphicsContainerDelegate Frame = WX.Frame ()
  getGraphicsContainer = getDelegate

instance ResizeEventSource Frame where
  registerResizeListener (Frame id) handler = 
    deferRegisterEventHandler id getFrameDelegate WX.resize (handler $ Resize id)

getFrameDelegate :: Integer -> Druid (Delegate Frame)
getFrameDelegate id = getWXWidget id >>= \(WXFrame w) -> return w

createFrame :: [Property Frame] -> Druid Frame
createFrame props = do
  id <- getNextId 
  {-createTopLevelWidget id (WX.frame props) WXFrame-}
  createTopLevelWidget id (WXExt.createFrame props) WXFrame
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
-- Text Field
---------------------------------------------------------------

data TextField = TextField Integer
  
instance Widget TextField where
  type Delegate TextField= WX.TextCtrl ()
  type Attribute TextField = WX.Attr (WX.TextCtrl ())
  type Property TextField = Prop (WX.TextCtrl ())
  getId (TextField id) = id
  getDelegate (TextField id) = getWXWidget id >>= \(WXTextField w) -> return w
  getProperty = getWidgetProperty
  setProperties w props = addUpdateOp $ setWidgetProperties w props
  remove w = addRemoveOp $ setWidgetProperties w [WX.visible := False]

getTextFieldDelegate :: Integer -> Druid (Delegate TextField)
getTextFieldDelegate id = getWXWidget id >>= \(WXTextField w) -> return w

createTextField :: Container c => c -> [Property TextField] -> Druid TextField
createTextField parent props = do
  id <- getNextId
  createControlWidget id (getId parent) (\w -> WX.textEntry w props) WXTextField
  return $ TextField id

textChange :: WX.Event (WXCore.Control a) (IO ())
textChange = WX.newEvent "textChange" (WXCore.controlGetOnText) (WXCore.controlOnText)

instance TextChangeEventSource TextField where
  registerTextChangeListener (TextField id) handler = 
    deferRegisterEventHandler id getTextFieldDelegate textChange (handler $ TextChange id)

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

instance Container Panel where
  type GraphicsContainerDelegate Panel = WX.Panel ()
  getGraphicsContainer = getDelegate

instance ResizeEventSource Panel where
  registerResizeListener (Panel id) handler = 
    deferRegisterEventHandler id getPanelDelegate WX.resize (handler $ Resize id)

getPanelDelegate :: Integer -> Druid (Delegate Panel)
getPanelDelegate id = getWXWidget id >>= \(WXPanel w) -> return w

createPanel :: Container c => c -> [Property Panel] -> Druid Panel
createPanel parent props = do
  id <- getNextId
  createControlWidget id (getId parent) (\w -> WXExt.createPanel w props) WXPanel
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
-- Rectangle
---------------------------------------------------------------

data Rectangle = Rectangle Integer

instance Widget Rectangle where
  type Delegate Rectangle = WXExt.Rectangle
  type Attribute Rectangle = WX.Attr WXExt.Rectangle
  type Property Rectangle = Prop WXExt.Rectangle
  getId (Rectangle id) = id
  getDelegate (Rectangle id) = getWXWidget id >>= \(WXRectangle w) -> return w
  getProperty = getWidgetProperty
  setProperties w props = addUpdateOp $ setWidgetProperties w props
  remove w = return () -- TODO: addRemoveOp $ setWidgetProperties w [WX.visible := False]

createRectangle :: Container c => c -> [Property Rectangle] -> Druid Rectangle
createRectangle parent props = do
  id <- getNextId
  createGraphicalWidget id parent (\w -> WXExt.createRectangle w props) WXRectangle
  return $ Rectangle id  

getRectangleDelegate :: Integer -> Druid (Delegate Rectangle)
getRectangleDelegate id = getWXWidget id >>= \(WXRectangle w) -> return w

---------------------------------------------------------------
-- Circle
---------------------------------------------------------------

data Ellipse = Ellipse Integer

instance Widget Ellipse where
  type Delegate Ellipse = WXExt.Ellipse
  type Attribute Ellipse = WX.Attr WXExt.Ellipse
  type Property Ellipse = Prop WXExt.Ellipse
  getId (Ellipse id) = id
  getDelegate (Ellipse id) = getWXWidget id >>= \(WXEllipse w) -> return w
  getProperty = getWidgetProperty
  setProperties w props = addUpdateOp $ setWidgetProperties w props
  remove w = return () -- TODO: addRemoveOp $ setWidgetProperties w [WX.visible := False]

createEllipse :: Container c => c -> [Property Ellipse] -> Druid Ellipse
createEllipse parent props = do
  id <- getNextId
  createGraphicalWidget id parent (\w -> WXExt.createEllipse w props) WXEllipse
  return $ Ellipse id  

getEllipseDelegate :: Integer -> Druid (Delegate Ellipse)
getEllipseDelegate id = getWXWidget id >>= \(WXEllipse w) -> return w

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

createGraphicalWidget :: (Container c) => Integer ->  c -> (forall w. WXExt.GraphicsContainer w => w -> IO a) -> (a -> WXWidget) -> Druid ()
createGraphicalWidget id parent delegate wrapper = addCreateOp $ do
  WXExt.AnyGraphicsContainer w <- getGraphicalContainer parent
  g <- liftIO $ delegate w
  storeDelegate id (wrapper g)  

setWidgetProperties :: Widget w => w -> [Property w] -> Druid ()
setWidgetProperties widget props = do
  wxObj <- getDelegate widget
  liftIO $ WX.set wxObj props

getWidgetProperty :: Widget w => w -> Attribute w a -> Druid a
getWidgetProperty widget attr = do
  wxObj <- getDelegate widget
  liftIO $ WX.get wxObj attr

getGraphicalContainer :: Widget w => w -> Druid WXExt.AnyGraphicsContainer
getGraphicalContainer widget = do
  wxwidget <- getWXWidget $ getId widget
  return $ case wxwidget of
             WXFrame w -> WXExt.AnyGraphicsContainer w
             WXPanel w -> WXExt.AnyGraphicsContainer w
             _         -> error "Not a graphics container"

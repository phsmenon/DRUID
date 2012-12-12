{-# LANGUAGE TypeFamilies, ExistentialQuantification, RankNTypes, ConstraintKinds #-}

module Druid.Controls where

import qualified Graphics.UI.WX as WX  hiding ((:=))
import Graphics.UI.WX(Prop((:=)), Attr(..))

import Control.Monad.IO.Class

import Druid.DruidMonad

-------------------------------------------------------------------------

data Frame = Frame Integer
data Button = Button Integer
data Label = Label Integer

---------------------------------------------------------------

 -- data AnyWindow = forall a. AnyWindow (WX.Window a)

 -- data Foo w = forall a. Window a ~ Delegate w :: Constraint

class (Property w ~ Prop (Delegate w), Attribute w ~ Attr (Delegate w)) => Widget w where
  type Delegate w :: *
  type Attribute w :: * -> *
  type Property w :: *
  getId :: w -> Integer
  getDelegate :: w -> Druid(Delegate w)
  getProperty :: w -> Attribute w a -> Druid a
  setProperties :: w -> [Property w] -> Druid ()
  setProperty :: w -> Property w -> Druid ()
  setProperty w prop = setProperties w [prop]

class Widget w => Container w where
  getDelegateContainer :: w -> Druid(Delegate w)

class Widget w => CommandEventSource w where
  registerCommandListener :: w -> (UIEvent -> IO ()) -> Druid ()

---------------------------------------------------------------

deferRegisterEventHandler :: b -> (b -> Druid w) -> WX.Event w a -> a -> Druid ()
deferRegisterEventHandler id lookup event handler = do
  let action wxobj = WX.set wxobj [WX.on event := handler]
  addUpdateOp $ lookup id >>= \v -> liftIO $ action v
  

createTopLevelWidget :: Integer -> IO a -> (a -> WXWidget) -> Druid ()
createTopLevelWidget id delegate wrapper = do
  w <- liftIO delegate
  stoveDelegate id (wrapper w)
  
createControlWidget :: Integer -> Integer -> ((WX.Frame ()) -> IO a) -> (a -> WXWidget) -> Druid ()
createControlWidget id parent delegate wrapper = do
  parentWindow <- getWXFrame parent
  w <- liftIO $ delegate parentWindow
  stoveDelegate id (wrapper w)  

{-setWidgetProperty :: Integer -> (Integer -> Druid w) -> [Prop w] -> Druid ()  -}
{-setWidgetProperty id lookup properties = do-}
  {-w <- lookup id-}
  {-liftIO $ WX.set w properties-}

setWidgetProperties :: Widget w => w -> [Property w] -> Druid ()
setWidgetProperties widget props = do
  wxObj <- getDelegate widget
  liftIO $ WX.set wxObj props

getWidgetProperty :: Widget w => w -> Attribute w a -> Druid a
getWidgetProperty widget attr = do
  wxObj <- getDelegate widget
  liftIO $ WX.get wxObj attr
  

-------------------------------------------------------------------------

createFrame :: [Property Frame] -> Druid Frame
createFrame props = do
  id <- getNextId 
  {-addCreateOp $ createTopLevelWidget id (WX.frame [WX.text := title]) WXFrame-}
  addCreateOp $ createTopLevelWidget id (WX.frame props) WXFrame
  return $ Frame id
  
getWXFrame :: Integer -> Druid (WX.Frame ())
getWXFrame id = do
  WXFrame w <- getWXWidget id
  return w

instance Widget Frame where
  type Delegate Frame = WX.Frame ()
  type Attribute Frame = WX.Attr (WX.Frame ())
  type Property Frame = Prop (WX.Frame ())
  getId (Frame id) = id
  getDelegate (Frame id) = getWXWidget id >>= \(WXFrame w) -> return w
  getProperty = getWidgetProperty
  setProperties w props = addUpdateOp $ setWidgetProperties w props

instance Container Frame where
  getDelegateContainer = getDelegate

---------------------------------------------------------------  

createLabel :: Frame -> [Property Label] -> Druid Label
createLabel (Frame parent) props = do
  id <- getNextId
  addCreateOp $ createControlWidget id parent (\w -> WX.staticText w props) WXLabel
  return $ Label id

getWXLabel :: Integer -> Druid (WX.StaticText ())
getWXLabel id = do
  WXLabel w <- getWXWidget id
  return w
  
instance Widget Label where
  type Delegate Label = WX.StaticText ()
  type Attribute Label = WX.Attr (WX.StaticText ())
  type Property Label = Prop (WX.StaticText ())
  getId (Label id) = id
  getDelegate (Label id) = getWXWidget id >>= \(WXLabel w) -> return w
  getProperty = getWidgetProperty
  setProperties w props = addUpdateOp $ setWidgetProperties w props

---------------------------------------------------------------  
  
createButton :: Frame -> [Property Button] -> Druid Button
createButton (Frame parent) props = do
  id <- getNextId
  addCreateOp $ createControlWidget id parent (\w -> WX.button w props) WXButton
  return $ Button id
  
getWXButton :: Integer -> Druid (WX.Button ())
getWXButton id = do
  WXButton w <- getWXWidget id
  return w
    
instance Widget Button where
  type Delegate Button = WX.Button ()
  type Attribute Button = WX.Attr (WX.Button ())
  type Property Button = Prop (WX.Button ())
  getId (Button id) = id
  getDelegate (Button id) = getWXWidget id >>= \(WXButton w) -> return w
  getProperty = getWidgetProperty
  setProperties w props = addUpdateOp $ setWidgetProperties w props

instance CommandEventSource Button where
  registerCommandListener (Button id) handler = deferRegisterEventHandler id getWXButton WX.command (handler $ Command id)


---------------------------------------------------------------
  
createInternalTimer :: Integer -> (UIEvent -> IO ()) -> Druid ()
createInternalTimer time callback = do
  f <- liftIO $ WX.frame  [WX.text := "Internal Window for Timer", WX.visible := False]
  liftIO $ WX.timer f [WX.interval := (fromIntegral time), WX.on WX.command := callback Heartbeat]
  return ()
  

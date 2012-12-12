{-# LANGUAGE TypeFamilies #-}

module Druid.Controls where

import qualified Graphics.UI.WX as WX  hiding ((:=))
import Graphics.UI.WX(Prop((:=)))

import Control.Monad.IO.Class

import Druid.DruidMonad

-------------------------------------------------------------------------

data Frame = Frame Integer
data Button = Button Integer
data Label = Label Integer

---------------------------------------------------------------

class Widget w where
  type Delegate w :: *
  type Property w :: *
  getId :: w -> Integer
  getDelegate :: w -> Druid(Delegate w)
  --setProperty w -> Property -> Druid ()

class (Widget w) => CommandEventSource w where
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

setWidgetProperty :: Integer -> (Integer -> Druid w) -> [Prop w] -> Druid ()  
setWidgetProperty id lookup properties = do
  w <- lookup id
  liftIO $ WX.set w properties

{-setProp :: Widget w => w -> [Property w] -> Druid ()-}
{-setProp widget props = do-}
  {-wxObj <- getWXWidget $ getId widget-}
  {-liftIO $ WX.set wxObj props-}

-------------------------------------------------------------------------

createFrame :: String -> Druid Frame
createFrame title = do
  id <- getNextId 
  addCreateOp $ createTopLevelWidget id (WX.frame [WX.text := title]) WXFrame
  return $ Frame id
  
getWXFrame :: Integer -> Druid (WX.Frame ())
getWXFrame id = do
  WXFrame w <- getWXWidget id
  return w

setFrameProperty :: Frame -> [Prop (WX.Frame ())] -> Druid ()
setFrameProperty (Frame id) props = addUpdateOp $ setWidgetProperty id getWXFrame props

instance Widget Frame where
  type Delegate Frame = WX.Frame ()
  type Property Frame = Prop (WX.Frame ())
  getId (Frame id) = id
  getDelegate (Frame id) = getWXWidget id >>= \(WXFrame w) -> return w
  -- setProperty w property = addUpdateOp $ setWidget

---------------------------------------------------------------  

createLabel :: Frame -> String -> Druid Label
createLabel (Frame parent) text = do
  id <- getNextId
  addCreateOp $ createControlWidget id parent (\w -> WX.staticText w [WX.text := text]) WXLabel
  return $ Label id
  
getWXLabel :: Integer -> Druid (WX.StaticText ())
getWXLabel id = do
  WXLabel w <- getWXWidget id
  return w
  
setLabelProperty :: Label -> [Prop (WX.StaticText ())] -> Druid ()
setLabelProperty (Label id) props = addUpdateOp $ setWidgetProperty id getWXLabel props
  
instance Widget Label where
  type Delegate Label = WX.StaticText ()
  type Property Label = Prop (WX.StaticText ())
  getDelegate (Label id) = getWXWidget id >>= \(WXLabel w) -> return w
  getId (Label id) = id

---------------------------------------------------------------  
  
createButton :: Frame -> String -> Druid Button
createButton (Frame parent) text = do
  id <- getNextId
  addCreateOp $ createControlWidget id parent (\w -> WX.button w [WX.text := text]) WXButton
  -- Very rough
  return $ Button id
  
getWXButton :: Integer -> Druid (WX.Button ())
getWXButton id = do
  WXButton w <- getWXWidget id
  return w
    
setButtonProperty :: Button -> [Prop (WX.Button ())] -> Druid ()
setButtonProperty (Button id) props = addUpdateOp $ setWidgetProperty id getWXButton props

instance Widget Button where
  type Delegate Button = WX.Button ()
  type Property Button = Prop (WX.Button ())
  getId (Button id) = id
  getDelegate (Button id) = getWXWidget id >>= \(WXButton w) -> return w

instance CommandEventSource Button where
  registerCommandListener (Button id) handler = deferRegisterEventHandler id getWXButton WX.command (handler $ Command id)


  

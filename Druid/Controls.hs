module Druid.Controls where

import qualified Graphics.UI.WX as WX  hiding ((:=))
import Graphics.UI.WX(Prop((:=)))

import Control.Monad.IO.Class

import Druid.DruidMonad

-------------------------------------------------------------------------
--
data Frame = Frame Integer
data Button = Button Integer
data Label = Label Integer

---------------------------------------------------------------

deferRegisterEventHandler :: b -> (b -> Druid w) -> WX.Event w a -> a -> Druid ()
deferRegisterEventHandler id lookup event handler = do
  let action wxobj = WX.set wxobj [WX.on event := handler]
  addUpdateOp $ lookup id >>= \v -> liftIO $ action v
  
standardEventReceiver :: UIEvent -> IO ()
standardEventReceiver e = 
  case e of 
    Command id -> putStrLn $ "Command event received for " ++ show id

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

registerButtonCommandEventHandler :: Button -> (UIEvent -> IO ()) -> Druid ()
registerButtonCommandEventHandler (Button id) handler = 
  deferRegisterEventHandler id getWXButton WX.command (standardEventReceiver $ Command id)
  

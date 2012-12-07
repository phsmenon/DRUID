module DruidMonad where

import Control.Monad.State

import Data.List
import Data.Maybe
import Debug.Trace

import qualified Graphics.UI.WX as WX  hiding ((:=))
import Graphics.UI.WX(Prop((:=)))

data WXWidget = 
    WXFrame (WX.Frame ())
  | WXButton (WX.Button ())
  | WXLabel (WX.StaticText ())
  
type WidgetDelegatePair = (Integer, WXWidget)

data DruidData = DruidData { 
    widgets :: [WidgetDelegatePair], 
    maxId :: Integer, 
    createOps :: [Druid ()],
    updateOps :: [Druid ()],
    removeOps :: [Druid ()]
}

initialDruidState = DruidData { 
  widgets = [], maxId = 1, createOps = [], updateOps = [], removeOps = [] 
}

type Druid = StateT DruidData IO

getNextId :: Druid Integer
getNextId = do
  r@DruidData { maxId = currentId } <- get
  put $ r { maxId = currentId + 1 }
  return currentId
  
addCreateOp :: Druid () -> Druid ()
addCreateOp op = do
  r@DruidData { createOps = ops } <- get
  put $ r { createOps = ops ++ [op] }
  return ()

addUpdateOp :: Druid () -> Druid ()
addUpdateOp op = do
  r@DruidData { updateOps = ops } <- get
  put $ r { updateOps = ops ++ [op] }
  return ()
  
addRemoveOp :: Druid () -> Druid ()
addRemoveOp op = do
  r@DruidData { removeOps = ops } <- get
  put $ r { removeOps = ops ++ [op] }
  return ()
  
stoveDelegate :: Integer -> WXWidget -> Druid ()
stoveDelegate id widget = do
  r@DruidData { widgets = wlist } <- get
  put $ r { widgets = (id,widget):wlist }
  return ()

-- TODO: Better error handling / messages
getWXWidget :: Integer -> Druid WXWidget 
getWXWidget id = do 
  DruidData { widgets = wlist } <- get
  let (Just (id', w)) = find (\(id', w) -> id' == id) wlist
  return w
  
createTopLevelWidget :: Integer -> IO a -> (a -> WXWidget) -> Druid ()
createTopLevelWidget id delegate wrapper = do
  w <- liftIO delegate
  stoveDelegate id (wrapper w)
  
createControlWidget :: Integer -> Integer -> ((WX.Frame ()) -> IO a) -> (a -> WXWidget) -> Druid ()
createControlWidget id parent delegate wrapper = do
  parentWindow <- getWXFrame parent
  w <- liftIO $ delegate parentWindow
  stoveDelegate id (wrapper w)  

doCreateOps :: Druid ()
doCreateOps = do
  r@DruidData { createOps = ops } <- get
  sequence_ ops
  put $ r { createOps = [] }  -- Clear the createOps, now that we are done with it

data Frame = Frame Integer
data Button = Button Integer
data Label = Label Integer

---------------------------------------------------------------

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
setFrameProperty (Frame id) props = do
  w <- getWXFrame id
  liftIO $ WX.set w props

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
setLabelProperty (Label id) props = do
  w <- getWXLabel id
  liftIO $ WX.set w props
  
---------------------------------------------------------------  
  
createButton :: Frame -> String -> Druid Button
createButton (Frame parent) text = do
  id <- getNextId
  addCreateOp $ createControlWidget id parent (\w -> WX.button w [WX.text := text]) WXButton
  return $ Button id
  
getWXButton :: Integer -> Druid (WX.Button ())
getWXButton id = do
  WXButton w <- getWXWidget id
  return w
    
setButtonProperty :: Button -> [Prop (WX.Button ())] -> Druid ()
setButtonProperty (Button id) props = do
  w <- getWXButton id
  liftIO $ WX.set w props  

---------------------------------------------------------------  

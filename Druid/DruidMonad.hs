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

data UIEvent = Command Integer

data DruidData = DruidData { 
    widgets :: [WidgetDelegatePair], 
    maxId :: Integer, 
    createOps, updateOps, removeOps :: [Druid ()]
}

instance Show DruidData where
  show r = let sWidgets = "Ids: " ++ (intercalate "," $ map (show.fst) $ widgets r) in
           let sMaxId = "Max Id: " ++ (show $ maxId r) in
           let sCreateOps = "Create op counts: " ++ (show . length $ createOps r) in
           let sUpdateOps = "Update op counts: " ++ (show . length $ updateOps r) in
           let sRemoveOps = "Remove op counts: " ++ (show . length $ removeOps r) in
           "{ " ++ intercalate ", " [sWidgets, sMaxId, sCreateOps, sUpdateOps, sRemoveOps] ++ " }"

initialDruidState = DruidData { 
  widgets = [], maxId = 1, createOps = [], updateOps = [], removeOps = [] 
}
 
type Druid = StateT DruidData IO

getNextId :: Druid Integer
getNextId = get >>= \r@DruidData { maxId = id } -> put r { maxId = id + 1 } >> return id
  
addCreateOp :: Druid () -> Druid ()
addCreateOp op = get >>= \r@DruidData { createOps = ops } -> put r { createOps = ops ++ [op] }

addUpdateOp :: Druid () -> Druid ()
addUpdateOp op = get >>= \r@DruidData { updateOps = ops } -> put r { updateOps = ops ++ [op] }
  
addRemoveOp :: Druid () -> Druid ()
addRemoveOp op = get >>= \r@DruidData { removeOps = ops } -> put r { removeOps = ops ++ [op] }

clearCreateOps :: Druid ()
clearCreateOps = get >>= \r -> put r { createOps = [] }

clearUpdateOps :: Druid ()
clearUpdateOps = get >>= \r -> put r { updateOps = [] }

clearRemoveOps :: Druid ()
clearRemoveOps = get >>= \r -> put r { removeOps = [] }
  
stoveDelegate :: Integer -> WXWidget -> Druid ()
stoveDelegate id widget = get >>= \r@DruidData { widgets = lst } -> put r { widgets = (id,widget):lst }

getWXWidget :: Integer -> Druid WXWidget 
getWXWidget id = do 
  r@DruidData { widgets = wlist } <- get
  let res = find (\(id', w) -> id' == id) wlist
  case res of 
    Nothing        -> traceStack "" $ error ("Object with id " ++ show id ++  " not in list.")
    Just (_, w)    -> return w
  
createTopLevelWidget :: Integer -> IO a -> (a -> WXWidget) -> Druid ()
createTopLevelWidget id delegate wrapper = do
  w <- liftIO delegate
  stoveDelegate id (wrapper w)
  
createControlWidget :: Integer -> Integer -> ((WX.Frame ()) -> IO a) -> (a -> WXWidget) -> Druid ()
createControlWidget id parent delegate wrapper = do
  parentWindow <- getWXFrame parent
  w <- liftIO $ delegate parentWindow
  stoveDelegate id (wrapper w)  

doOps :: Druid ()
doOps = do
  DruidData { createOps = cOps, updateOps = uOps, removeOps = rOps } <- get
  -- get >>= \x -> liftIO $ traceIO (show x)
  liftIO $ traceIO ("To create: " ++ (show $ length $ cOps ++ uOps ++ rOps))
  sequence_ $ cOps ++ uOps ++ rOps
  clearCreateOps >> clearUpdateOps >> clearRemoveOps
  -- get >>= \x -> liftIO $ traceIO (show x)

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
  -- Very rough
  deferRegisterEventHandler id getWXButton WX.command (standardEventReceiver $ Command id)
  return $ Button id
  
getWXButton :: Integer -> Druid (WX.Button ())
getWXButton id = do
  WXButton w <- getWXWidget id
  return w
    
setButtonProperty :: Button -> [Prop (WX.Button ())] -> Druid ()
setButtonProperty (Button id) props = do
  w <- getWXButton id
  liftIO $ WX.set w props  

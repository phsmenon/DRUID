module Druid.DruidMonad where

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
  deriving (Eq, Show)

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
  

doOps :: Druid ()
doOps = do
  DruidData { createOps = cOps, updateOps = uOps, removeOps = rOps } <- get
  -- get >>= \x -> liftIO $ traceIO (show x)
  liftIO $ traceIO ("To create: " ++ (show $ length $ cOps ++ uOps ++ rOps))
  sequence_ $ cOps ++ uOps ++ rOps
  clearCreateOps >> clearUpdateOps >> clearRemoveOps
  -- get >>= \x -> liftIO $ traceIO (show x)


---------------------------------------------------------------


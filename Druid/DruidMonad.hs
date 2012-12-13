{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ExistentialQuantification #-}

module Druid.DruidMonad where

import Control.Monad.State
import Control.Monad
import Control.Applicative

import Data.List
import Data.Maybe
import Debug.Trace
import Data.IORef

import qualified Graphics.UI.WX as WX  hiding ((:=))
import Graphics.UI.WX(Prop((:=)))

------------------------------------------------------------------
-- Data Types for the Engine
------------------------------------------------------------------

type Stimulus = (Double, Maybe UIEvent)

data Behavior a = Behavior (Stimulus -> Druid (Behavior a, a))

data Event a = Event (Stimulus -> Druid (Event a, Maybe a))


class Num a => Vec a where
  (*^) :: Double -> a -> a

------------------------------------------------------------------

data WXWidget = 
    WXFrame (WX.Frame ())
  | WXButton (WX.Button ())
  | WXLabel (WX.StaticText ())
  | WXPanel (WX.Panel ())

data WXWindow = forall a. WXWindow (WX.Window a)
  
type Tag = String

type WidgetDelegatePair = (Maybe Tag, (Integer, WXWidget))

data UIEvent = 
  Heartbeat 
  | Command Integer
  deriving (Eq, Show)

data DruidData = DruidData { 
    widgets :: [WidgetDelegatePair], 
    maxId :: Integer, 
    createOps, updateOps, removeOps :: [Druid ()],
    stepperDataRef :: StepperDataRef,
    timeStep :: Double
}

type StepperDataRef = IORef (Maybe (DruidData, Behavior (Druid ())))

instance Show DruidData where
  show r = let sWidgets = "Ids: " ++ (intercalate "," $ map (show.fst) $ widgets r) in
           let sMaxId = "Max Id: " ++ (show $ maxId r) in
           let sCreateOps = "Create op counts: " ++ (show . length $ createOps r) in
           let sUpdateOps = "Update op counts: " ++ (show . length $ updateOps r) in
           let sRemoveOps = "Remove op counts: " ++ (show . length $ removeOps r) in
           let sLastTimeStep = "Last time step: " ++ (show $ timeStep r) in
           "{ " ++ intercalate ", " [sWidgets, sMaxId, sLastTimeStep, sCreateOps, sUpdateOps, sRemoveOps] ++ " }" 

 
type Druid = StateT DruidData IO


initializeDruidData :: IO DruidData
initializeDruidData = do
  ref <- newIORef Nothing
  return $ DruidData { widgets = [], maxId = 1, timeStep = 0, createOps = [], updateOps = [], removeOps = [], stepperDataRef = ref}

getNextId :: Druid Integer
getNextId = get >>= \r@DruidData { maxId = id } -> put r { maxId = id + 1 } >> return id
  
getTimeStep :: Druid Double
getTimeStep = get >>= \r -> return $ timeStep r

putTimeStep :: Double -> Druid ()
putTimeStep time = get >>= \r -> put r { timeStep = time }

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

storeDelegate :: Integer -> WXWidget -> Druid ()
storeDelegate id widget = get >>= \r@DruidData { widgets = lst } -> 
    put r { widgets = (Nothing, (id,widget)):lst }

storeDelegateTag :: Tag -> Integer -> WXWidget -> Druid ()
storeDelegateTag tag id widget = get >>= \r@DruidData { widgets = lst } -> 
    put r { widgets = (Just tag, (id,widget)):lst }

getWXWidget :: Integer -> Druid WXWidget 
getWXWidget id = do 
  r@DruidData { widgets = wlist } <- get
  let res = find (\w -> (fst $ snd w) == id) wlist
  case res of 
    Nothing          -> traceStack "" $ error ("Object with id " ++ show id ++ " not in list.")
    Just (_, (_, w)) -> return w

getWidgetsByTag :: Tag -> Druid [WXWidget]
getWidgetsByTag tag = do
  r@DruidData { widgets = wlist } <- get
  let res = map (\w -> snd . snd $ w) $ filter (\w -> maybe False (== tag) $ fst w) wlist
  case res of 
    [] -> traceStack "" $ error ("Objects with tag " ++ show tag ++ " not in list.")
    xs -> return xs

getWXWindow :: Integer -> Druid WXWindow 
getWXWindow id = do
  widget <- getWXWidget id
  return $ case widget of
             WXFrame w -> WXWindow w
             WXLabel w -> WXWindow w
             WXButton w -> WXWindow w
             WXPanel w -> WXWindow w
  
runDruid :: Druid a -> DruidData -> IO (a, DruidData)
runDruid druidOp state = runStateT druidOp state

execDruid :: Druid () -> DruidData -> IO DruidData
execDruid druidOp state = execStateT druidOp state

doOps :: Druid ()
doOps = do
  DruidData { createOps = cOps, updateOps = uOps, removeOps = rOps } <- get
  -- get >>= \x -> liftIO $ traceIO (show x)
  -- liftIO $ traceIO ("To create: " ++ (show $ length $ cOps ++ uOps ++ rOps))
  sequence_ $ cOps ++ uOps ++ rOps
  clearCreateOps >> clearUpdateOps >> clearRemoveOps
  -- get >>= \x -> liftIO $ traceIO (show x)


---------------------------------------------------------------


{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, TypeFamilies #-}

module Druid.DruidMonad where

import Control.Monad.State
import Control.Monad
import Control.Applicative

import Data.List
import Data.Maybe
import Debug.Trace
import Data.IORef

import Data.Typeable

import qualified Graphics.UI.WX as WX  hiding ((:=))
import Graphics.UI.WX(Prop((:=)))

import qualified Druid.WXExtensions as WXExt

------------------------------------------------------------------
-- Data Types for the Engine
------------------------------------------------------------------

data Stimulus = Stimulus Double

data Behavior a = Behavior (Stimulus -> Druid (Behavior a, a))

data Event a = Event (Stimulus -> Druid (Event a, Maybe a))

------------------------------------------------------------------

class ReactiveProxy w where
  type Attribute w :: * -> *
  type CreateAttr w :: *
  getName :: w -> Druid String
  create  :: CreateAttr w -> Druid w
  getAttr :: Typeable a => w -> Attribute w a -> Druid (Behavior a)
  setAttr :: Typeable a => w -> Attribute w a -> Behavior a -> Druid ()
  -- removeObject :: w -> Druid ()
  react   :: w -> Event e -> (w -> e -> Druid ()) -> Druid ()

class ReactiveProxy w => ReactiveWidget w where
  updateAttributes :: Stimulus -> w -> Druid () 

data AnyReactive= forall a. ReactiveWidget a => AnyReactive a

------------------------------------------------------------------

data DruidData = DruidData { 
    widgets :: [AnyReactive], 
    maxId :: Integer, 
    updateOps :: [Druid ()],
    stepperDataRef :: StepperDataRef,
    reactors :: [Behavior ()]
}

type StepperDataRef = IORef (Maybe (DruidData))

instance Show DruidData where
  show r = let sWidgets = "Widget count: " ++ (show . length $ widgets r) in
           {-let sReactives = "Reactives Count: " ++ (show .length $ reactives r) in-}
           let sUpdateOps = "Update op counts: " ++ (show . length $ updateOps r) in
           let sReactors = "Reactor Count: " ++ (show . length $ reactors r) in
           {-let sLastTimeStep = "Last time step: " ++ (show $ timeStep r) in-}
           "{ " ++ intercalate ", " [  sWidgets,  {-sReactives,-} {- sLastTimeStep, ,-} sUpdateOps, sReactors] ++ " }" 

 
type Druid = StateT DruidData IO

------------------------------------------------------------------

initializeDruidData :: IO DruidData
initializeDruidData = do
  ref <- newIORef Nothing
  return $ DruidData { widgets = [], maxId = 1, {-timeStep = 0,-} updateOps = [], stepperDataRef = ref, reactors = []}

addReactive :: ReactiveWidget w => w -> Druid ()
addReactive w = get >>= \r -> put r { widgets = (AnyReactive w):(widgets r) }

getReactives :: Druid [AnyReactive]
getReactives = get >>= \r -> return $ widgets r

deferUpdateOp :: Druid () -> Druid ()
deferUpdateOp update = get >>= \r -> put r { updateOps = update:(updateOps r) }

doDeferredOps :: Druid ()
doDeferredOps = get >>= \r -> return (updateOps r) >>= sequence_ >> put r { updateOps = [] }

getStepperDataRef :: Druid StepperDataRef
getStepperDataRef = get >>= return . stepperDataRef

getReactors :: Druid [Behavior ()]
getReactors = get >>= \r -> return $ reactors r

clearReactors :: Druid ()
clearReactors = get >>= \r -> put r { reactors = [] }

addReactors :: [Behavior ()] -> Druid ()
addReactors rs = get >>= \r -> put r { reactors = (reactors r) ++ rs }

traceDruidData :: Druid ()
traceDruidData = get >>= liftIO . traceIO . show

traceDruidDataMsg :: String -> Druid ()
traceDruidDataMsg msg = do
  text <- get >>= return . show 
  let text' = msg ++ text
  liftIO $ traceIO text'

runDruid :: Druid a -> DruidData -> IO (a, DruidData)
runDruid = runStateT 

execDruid :: Druid () -> DruidData -> IO DruidData
execDruid = execStateT


{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, TypeFamilies, ConstraintKinds, GADTs #-}

module Druid.WX.DruidMonad (
  module Druid.Engine,
  module Druid.EngineMonad,
  module Druid.WX.DruidMonad
) where

import Control.Monad.State.Strict
import Control.Monad
import Control.Applicative

import Data.List
import Data.Maybe
import Debug.Trace
import Data.IORef
import Text.Printf
import qualified Data.Map as M

import Data.Typeable
import GHC.Exts (Constraint)

import Druid.EngineMonad
import Druid.Engine
import Druid.WX.Types

------------------------------------------------------------------

type Behavior a = SignalC Druid a

type Event a = EventC Druid a

------------------------------------------------------------------

class ReactiveProxy w where
  type Delegate w :: *
  type Attribute w :: * -> *
  type CreateAttr w :: *
  type AttributeC w :: * -> Constraint
  type AttributeC w = Typeable
  type ReactorMonad w :: * -> *
  getDelegate :: w -> Delegate w
  getName :: w -> String
  create  :: CreateAttr w -> ReactorMonad w w
  getAttr :: AttributeC w a => w -> Attribute w a -> ReactorMonad w (Behavior a)
  setAttr :: AttributeC w a => w -> Attribute w a -> Behavior a -> ReactorMonad w ()
  remove  :: w -> ReactorMonad w ()
  now     :: AttributeC w a => w -> Attribute w a -> ReactorMonad w a
  react   :: w -> Event e -> (w -> e -> ReactorMonad w ()) -> ReactorMonad w ()

class ReactiveProxy w => ReactiveWidget w where
  updateAttributes :: Stimulus -> w -> Druid () 

data AnyReactive = forall a. ReactiveWidget a => AnyReactive a

------------------------------------------------------------------

data Reactor = forall e. Reactor (Event e) (e -> Druid ()) 

data UpdateOpKey = UpdateOpKey String String 
  deriving Eq

------------------------------------------------------------------

data DruidData = DruidData { 
    widgets :: [AnyReactive], 
    liveness :: M.Map String Bool,
    maxId :: Int, 
    updateOps :: [(UpdateOpKey, Druid ())],
    stepperDataRef :: StepperDataRef,
    reactors :: [Reactor],
    eventQueue :: IORef [WXEvent],
    inProgress :: IORef Bool
} deriving Typeable

type StepperDataRef = IORef (Maybe (DruidData))

instance Show DruidData where
  show r = let sWidgets = "Widget count: " ++ (show . length $ widgets r) in
           let sUpdateOps = "Update op counts: " ++ (show . length $ updateOps r) in
           let sReactors = "Reactor Count: " ++ (show . length $ reactors r) in
           let sLiveness = "Liveness State:" ++ (M.foldlWithKey (\res k v -> res ++ "\n  " ++ k ++ ": " ++ (show v)) "" (liveness r)) in
           -- let sEventQueue = "Event Queue State: " ++ map ((++ " ") . show) (eventQueue )
           -- let sInProgress = "In Progress: " ++ (show $ inProgress r) in
           "{ " ++ intercalate ", " [  sWidgets,  sUpdateOps, sReactors, sLiveness] ++ " }" 
 
type Druid = StateT DruidData IO

------------------------------------------------------------------

instance EngineMonad Druid where
  updateProxyAttributes st = getReactives >>= mapM_ (\(AnyReactive r) -> updateAttributes st r)

  executeReactors st = do
    -- This is a bit strange because of 2 things:
    --   1. Events can evolve
    --   2. Reactors, while executing, can add other reactors
    current <- getReactors
    clearReactors
    next' <- mapM runReactor current
    addReactors next'
    where
      runReactor (Reactor (Event ef) fn) = do
        -- If event occurred execute action; in all cases
        -- return a new reactor with the "aged" event
        (ev', res) <- ef st
        let reactor' = Reactor ev' fn
        maybe (return reactor') (\e -> fn e >> return reactor') res

  performDeferredOperations = get >>= return . (map snd) . updateOps >>= sequence_

  finalizeStep _ = modify ( \r -> r { updateOps = [] } )


instance EngineMonadIO Druid where
  data EngineMonadData Druid = DruidDataWrapper DruidData
  runEngineMonadIO (DruidDataWrapper dta) op = execStateT op dta >>= return . DruidDataWrapper

------------------------------------------------------------------

stepEngineIORef :: IORef (Maybe DruidData) -> IO ()
stepEngineIORef ref = do
  -- Read the druid data ref and ensure that it is non-Nothing. It shouldn't be in
  -- at this point unless there is a bug
  dta <- readIORef ref
  assertDruidData dta
  let druidData = fromJust dta
  -- Assert that we are not in a step operation already. Once again it is an internal error
  -- if we are (likely a failure of the event queuing scheme). Then set the in progress state
  -- and call the "real" stepper
  assertInProgressState druidData
  setInProgressState druidData True
  DruidDataWrapper st' <- stepEngineIO $ DruidDataWrapper druidData
  writeIORef ref (Just st')
  setInProgressState druidData False
  where
    assertDruidData maybeDruidData = if isNothing maybeDruidData then (error "Internal error: DruidData should not be NULL inside handleEvent") else return ()
    assertInProgressState druidData = readIORef (inProgress druidData) >>= \v -> if v then error "Internal Error: stepEngineIORef called when a step is already in progress" else return ()
    setInProgressState druidData st = writeIORef (inProgress druidData) st


-- Engine startup. Performes some bootstrapping
startEngine :: Druid() -> IO ()
startEngine startup = do
  druidData <- initializeDruidData
  let ref = stepperDataRef druidData
  let druidData' = druidData { reactors = [initialAction] }
  writeIORef ref $ Just druidData'
  {-liftIO . traceIO $ "Before start"-}
  stepEngineIORef $ stepperDataRef druidData'
  {-liftIO . traceIO $ "After start"-}
  where
    initialAction = Reactor (once $ always ()) (const startup)

------------------------------------------------------------------

initializeDruidData :: IO DruidData
initializeDruidData = do
  ref <- newIORef Nothing
  evQRef <- newIORef []
  inProgressRef <- newIORef False
  return $ DruidData { 
    widgets = [], maxId = 1, liveness = M.empty, updateOps = [], 
    stepperDataRef = ref, reactors = [], eventQueue = evQRef, 
    inProgress = inProgressRef }

addReactive :: ReactiveWidget w => w -> Druid ()
addReactive w = get >>= \r -> put r { widgets = (AnyReactive w):(widgets r) }

getReactives :: Druid [AnyReactive]
getReactives = get >>= return . widgets

setLiveness :: String -> Bool -> Druid ()
setLiveness name live = do 
  get >>= \r -> put r { liveness = M.insert name live (liveness r) }

isLive :: String -> Druid Bool
isLive name = get >>= return . liveness >>= return . fromJust . M.lookup name

deferUpdateOp :: String -> String -> Druid () -> Druid ()
deferUpdateOp wName attrName update = checkOverwrite >> modify (\r -> r { updateOps = (key, update):(updateOps r) } ) where
  checkOverwrite = isCurrentUpdateOp wName attrName >>= \res -> if res then warn else return ()
  warn = liftIO $ printf "Warning: Multiple updates on the same widget/attribute - %s, %s" wName attrName
  key = UpdateOpKey wName attrName

isCurrentUpdateOp :: String -> String -> Druid Bool
isCurrentUpdateOp wName attrName = do
  let key = UpdateOpKey wName attrName
  get >>= return . updateOps >>= return . isJust . lookup key


getStepperDataRef :: Druid StepperDataRef
getStepperDataRef = get >>= return . stepperDataRef

getReactors :: Druid [Reactor]
getReactors = get >>= \r -> return $ reactors r

clearReactors :: Druid ()
clearReactors = get >>= \r -> put r { reactors = [] }

addReactors :: [Reactor] -> Druid ()
addReactors rs = get >>= \r -> put r { reactors = (reactors r) ++ rs }

getEventQueueRef :: Druid (IORef [WXEvent])
getEventQueueRef = get >>= return . eventQueue

getInProgressRef :: Druid (IORef Bool) 
getInProgressRef = get >>= return . inProgress

traceDruidData :: Druid ()
traceDruidData = get >>= liftIO . traceIO . show

traceDruidDataMsg :: String -> Druid ()
traceDruidDataMsg msg = do
  text <- get >>= return . show 
  let text' = msg ++ text
  liftIO $ traceIO text'

getNextId :: Druid Int
getNextId = do 
  r <- get
  let current = maxId r
  put r { maxId = current + 1 }
  return current

getLatestEvent :: Druid (Maybe WXEvent)
getLatestEvent = do 
  queue <- getEventQueueRef >>= liftIO . readIORef
  liftIO $ if null queue then return Nothing else return $ Just (head queue)

runDruid :: Druid a -> DruidData -> IO (a, DruidData)
runDruid = runStateT 


{-# LANGUAGE TypeFamilies, ExistentialQuantification, ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Druid.Controls where

import qualified Graphics.UI.WX as WX  hiding ((:=))
import Graphics.UI.WX(Prop((:=)))

import qualified Graphics.UI.WXCore as WXCore

import qualified Druid.WXExtensions as WXExt

import Control.Monad.IO.Class

import Druid.Engine
import Druid.DruidMonad

import System.Random
import Data.IORef 
import System.IO.Unsafe

import Data.List
import Data.Maybe
import Data.Dynamic
import Control.Applicative
import qualified Data.HashTable.IO as H

import Debug.Trace

-------------------------------------------------------------------------------------------------

instance Num (WX.Point) where
   WX.Point x1 y1 + WX.Point x2 y2  = WX.Point (x1+x2) (y1+y2)
   WX.Point x1 y1 - WX.Point x2 y2  = WX.Point (x1 - x2) (y1 - y2)
   negate (WX.Point x y)      = WX.Point (-x) (-y)
   (*)                  = error "No * method for WX.Point"
   abs                  = error "No abs method for WX.Point"
   signum               = error "No * method for WX.Point"
   fromInteger 0        = WX.Point 0 0
   fromInteger _        = error "Only the constant 0 can be used as a WX.Point"

instance Vec WX.Point where
  d *^ (WX.Point x y) = WX.Point (round $ d*x') (round $ d*y')
    where x' = fromIntegral x
          y' = fromIntegral y

p2 :: Behavior Int -> Behavior Int -> Behavior (WX.Point)
p2 = lift2 WX.point

------------------------------------------------------------------------------------------------------------

deriving instance Typeable1 Behavior  -- Slightly unfortunate that this is required

data CachedProperty a = Typeable a => CachedProperty (Maybe (Double, a)) (Maybe (Behavior a))

data AnyCachedProperty = forall a. Typeable a => AnyCachedProperty (Maybe (Double, a)) (Maybe (Behavior a))

type PropertyCache = H.BasicHashTable String AnyCachedProperty


toCachedProperty :: Typeable a => AnyCachedProperty -> CachedProperty a
toCachedProperty (AnyCachedProperty cc beh) = CachedProperty (fromJust $ gcast cc) (fromJust $ gcast beh)

getFromCache :: Typeable a => PropertyCache -> WX.Attr w a -> Druid (Maybe (CachedProperty a))
getFromCache cache attr =
    liftIO $ H.lookup cache (WX.attrName attr) >>= return . toMaybeCachedProperty
    

toMaybeCachedProperty :: Typeable a => Maybe AnyCachedProperty -> Maybe (CachedProperty a)
toMaybeCachedProperty = maybe Nothing (Just . toCachedProperty) 
    
getFromCacheWithDefault :: Typeable a => PropertyCache -> WX.Attr w a -> CachedProperty a -> Druid (CachedProperty a)
getFromCacheWithDefault cache attr def = getFromCache cache attr >>= maybe (return def) return
    
updateCache :: Typeable a => PropertyCache -> WX.Attr w a -> CachedProperty a -> Druid ()
updateCache cache attr (CachedProperty cp beh) = liftIO $ H.insert cache (WX.attrName attr) (AnyCachedProperty cp beh)

updateAssociatedBehavior :: Typeable a => PropertyCache -> WX.Attr w a -> Behavior a -> Druid ()
updateAssociatedBehavior cache attr beh = do
    CachedProperty cv _ <- getFromCacheWithDefault cache attr (CachedProperty Nothing Nothing)
    -- traceDruidDataMsg $ "Updating behavior: "
    updateCache cache attr $ CachedProperty cv (Just beh)

------------------------------------------------------------------------------------------------------------

data AnyAttributeBehavior = forall a. Typeable a => AnyAttributeBehavior (Behavior a)

type AttributeBehaviorCache = H.BasicHashTable String AnyAttributeBehavior


getOrAddBehavior :: Typeable a => AttributeBehaviorCache -> WX.Attr w a -> Behavior a -> Druid (Behavior a)
getOrAddBehavior cc attr beh = do
  liftIO $ H.lookup cc name >>= maybe doInsert (\(AnyAttributeBehavior v) -> return . fromJust . cast $ v)
  where
    name = WX.attrName attr
    doInsert = H.insert cc name (AnyAttributeBehavior beh) >> return beh

processAttributeBehaviors :: AttributeBehaviorCache -> (forall a. Behavior a -> Druid ()) -> Druid ()
processAttributeBehaviors cache fn = do
  ab <- liftIO $ H.toList cache >>= return . map snd
  -- liftIO . traceIO $ "Am processing cached attribute behaviors: " ++ (show . length $ ab)
  sequence_ $ map (\(AnyAttributeBehavior v) -> fn v) ab

------------------------------------------------------------------------------------------------------------

makeAttributeBehavior :: Typeable a => w -> PropertyCache -> WX.Attr w a -> Behavior a
makeAttributeBehavior w cache attr = Behavior f where
    f s@(Stimulus t) = do
        CachedProperty cv beh <- getFromCacheWithDefault cache attr (CachedProperty Nothing Nothing)
        {-liftIO . traceIO $ "In behavior update: " ++ (show t)-}
        if (isJust cv && fst (fromJust cv) == t) 
            then do
                {-liftIO . traceIO $ "Returning cached behavior"-}
                return $ (Behavior f, fromJust . cast $ snd (fromJust cv))
            else do
                {-liftIO . traceIO $ "Not in cached behavior"-}
                value <- if (isJust beh) 
                            then executeBehavior w attr (fromJust beh) s 
                            else defaultBehavior w attr s
                return (Behavior f, value)
    attributeName = WX.attrName attr
    executeBehavior :: Typeable a => w -> WX.Attr w a -> (Behavior a) -> Stimulus -> Druid a
    executeBehavior w attr beh s@(Stimulus t) = do
        {-liftIO . traceIO $ "Executing behavior"-}
        (newBeh, av) <- stepBehavior s beh
        liftIO $ WX.set w [attr := av]  -- Write back to control
        liftIO $ H.insert cache attributeName (AnyCachedProperty (Just (t, av)) (Just newBeh)) --Update Beh
        return av
    defaultBehavior :: Typeable a => w -> WX.Attr w a -> Stimulus -> Druid a
    defaultBehavior w attr (Stimulus t) = do
        {-liftIO . traceIO $ "Doing Default Behavior"-}
        value <- liftIO $ WX.get w attr
        liftIO $ H.insert cache attributeName (AnyCachedProperty (Just (t, value)) Nothing) -- Update Beh
        return value            

------------------------------------------------------------------------------------------------------------

data AnyWXWindow = forall a. AnyWXWindow (WX.Window a)

class Container w where
  getWXWindow :: w -> AnyWXWindow 

data AnyContainer = forall w. Container w => AnyContainer w

instance Container AnyContainer  where
  getWXWindow (AnyContainer c) = getWXWindow c

-------------------------------------------------------------------------------------------------

createWindow :: ReactiveWidget a => IO a -> Druid a
createWindow constructor = liftIO constructor >>= \w -> addReactive w >> return w

updateAllAttributes :: Stimulus -> AttributeBehaviorCache -> Druid ()
updateAllAttributes st cc = processAttributeBehaviors cc (\v -> stepBehavior st v >> return ())

standardEventReceiver :: WXEvent -> Druid(IO ())
standardEventReceiver ev = do
  ref <- getStepperDataRef
  return $ addEvent ev >> {- liftIO (traceIO "Event handling") >> -} stepEngineIO ref >> popEvent

-------------------------------------------------------------------------------------------------

-- Crude. Will be fixed with factorizing Druid
eventQueue :: IORef [WXEvent]
{-# NOINLINE eventQueue #-}
eventQueue = unsafePerformIO $ newIORef []

addEvent :: WXEvent -> IO ()
addEvent ev = modifyIORef eventQueue (++ [ev])

popEvent :: IO ()
popEvent = modifyIORef eventQueue tail

getLatestEvent :: Druid (Maybe WXEvent)
getLatestEvent = liftIO $ readIORef eventQueue >>= \q -> if null q then return Nothing else return $ Just (head q)

-------------------------------------------------------------------------------------------------

data WXEvent = WXCommand Int
  deriving Eq

class CommandEventSource w where
  onCommand :: w -> Druid (Event ())

fromWXEvent :: (WXEvent -> Maybe v) -> Event v
fromWXEvent fn = f
  where f = Event(\_ -> getLatestEvent  >>= \ev -> return (f, maybe Nothing fn ev))

-------------------------------------------------------------------------------------------------

data WXFrame = WXFrame (WX.Frame ()) AttributeBehaviorCache PropertyCache

instance Container WXFrame where
  getWXWindow (WXFrame w _ _) = AnyWXWindow w

instance ReactiveProxy WXFrame where
  type Attribute WXFrame = WX.Attr (WX.Frame ())
  type CreateAttr WXFrame = ()
  getName (WXFrame w _ _) = liftIO $ WX.get w WX.identity >>= return . show
  getAttr (WXFrame w cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior w cc' attr
  setAttr f@(WXFrame _ _ cc') attr beh = deferUpdateOp $ updateAssociatedBehavior cc' attr beh >> getAttr f attr >> return ()
  create _ = createWindow $ WXFrame <$> WXExt.createFrame [] <*> H.new <*> H.new
  react w ev reactor = addReactors [reaction ev (reactor w)]

instance ReactiveWidget WXFrame where
  updateAttributes st (WXFrame _ cc _) = updateAllAttributes st cc

createWXFrame :: Druid WXFrame
createWXFrame = create ()

-------------------------------------------------------------------------------------------------

data WXButton = WXButton (WX.Button ()) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXButton where
  type Attribute WXButton = WX.Attr (WX.Button ())
  type CreateAttr WXButton = AnyContainer
  getName (WXButton w _ _ _) = liftIO $ WX.get w WX.identity >>= return . show
  getAttr (WXButton w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior w cc' attr
  setAttr b@(WXButton w _ cc cc') attr beh = deferUpdateOp $ updateAssociatedBehavior cc' attr beh >> getAttr b attr >> return ()
  create (AnyContainer parent) = do
    AnyWXWindow w <- return $ getWXWindow parent
    button <- liftIO $ WX.button w []
    buttonId <- liftIO $ WX.get button WX.identity
    attrCache <- liftIO $ H.new
    connCache <- liftIO $ H.new
    createWindow . return $ WXButton button buttonId attrCache connCache
  react w ev reactor = addReactors [reaction ev (reactor w)]

instance ReactiveWidget WXButton where
  updateAttributes st (WXButton _ _ cc _) = updateAllAttributes st cc

instance CommandEventSource WXButton where
  onCommand (WXButton button buttonId _ _) = registerListener >> return (fromWXEvent eventFilter)
    where
      registerListener = eventTransformer >>= \fn -> liftIO $ WX.set button [WX.on WX.command := fn]
      eventTransformer = standardEventReceiver $ WXCommand buttonId
      eventFilter (WXCommand id') | buttonId == id' = Just ()
      eventFilter _ = Nothing
    
createWXButton :: Container c => c -> Druid WXButton
createWXButton parent = create (AnyContainer parent)

-------------------------------------------------------------------------------------------------

data WXLabel = WXLabel (WX.StaticText ()) AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXLabel where
  type Attribute WXLabel = WX.Attr (WX.StaticText ())
  type CreateAttr WXLabel = AnyContainer
  getName (WXLabel w  _ _) = liftIO $ WX.get w WX.identity >>= return . show
  getAttr (WXLabel w cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior w cc' attr
  setAttr l@(WXLabel _ _ cc') attr beh = deferUpdateOp $ updateAssociatedBehavior cc' attr beh >> getAttr l attr >> return ()
  create (AnyContainer parent) = do
    AnyWXWindow w <- return $ getWXWindow parent
    createWindow $ WXLabel <$> WX.staticText w [] <*> H.new <*> H.new
  react w ev reactor = addReactors [reaction ev (reactor w)]

instance ReactiveWidget WXLabel where
  updateAttributes st (WXLabel _ cc _) = updateAllAttributes st cc

createWXLabel :: Container c => c -> Druid WXLabel
createWXLabel parent = create (AnyContainer parent)

-------------------------------------------------------------------------------------------------

data WXTimer = WXTimer (WX.Timer) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXTimer where
  type Attribute WXTimer = WX.Attr (WX.Timer)
  type CreateAttr WXTimer = (AnyContainer, Int)
  getName (WXTimer _ wid _ _) = return $ show wid
  getAttr (WXTimer w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior w cc' attr
  setAttr t@(WXTimer _ _ _ cc') attr beh = deferUpdateOp $ updateAssociatedBehavior cc' attr beh >> getAttr t attr >> return ()
  create (AnyContainer parent, tm) = do
    AnyWXWindow w <- return $ getWXWindow parent
    createWindow $ WXTimer <$> WX.timer w [WX.interval := tm] <*> liftIO randomIO <*> H.new <*> H.new
  react w ev reactor = addReactors [reaction ev (reactor w)]

instance ReactiveWidget WXTimer where
  updateAttributes st (WXTimer _ _ cc _) = updateAllAttributes st cc

instance CommandEventSource WXTimer where
  onCommand (WXTimer timer timerId _ _) = registerListener >> return (fromWXEvent eventFilter)
    where
      registerListener = eventTransformer >>= \fn -> liftIO $ WX.set timer [WX.on WX.command := fn]
      eventTransformer = standardEventReceiver $ WXCommand timerId
      eventFilter (WXCommand id') | timerId == id' = Just ()
      eventFilter _ = Nothing
    
createWXTimer :: Container c => c -> Int -> Druid WXTimer
createWXTimer parent interval = create (AnyContainer parent, interval)


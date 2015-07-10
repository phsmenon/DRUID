{-# LANGUAGE TypeFamilies, ExistentialQuantification, ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, RankNTypes, BangPatterns, ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TupleSections, UndecidableInstances #-}

module Druid.WX.Controls where

import qualified Graphics.UI.WX as WX  hiding ((:=))
import Graphics.UI.WX(Prop((:=)))
import qualified Graphics.UI.WXCore as WXC

import qualified Graphics.UI.WXCore as WXCore

import qualified Druid.WXExtensions as WXExt

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent

import Druid.Engine
import Druid.WX.DruidMonad
import Druid.WX.Cache
import Druid.WX.ReactiveUI
import Druid.WX.Types

import System.Random
import Data.IORef 
import System.IO.Unsafe

import Data.Maybe
import Data.Dynamic
import Control.Applicative
import Text.Printf
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

makeAttributeBehavior :: Typeable a => String -> w -> PropertyCache -> WX.Attr w a -> Behavior a
makeAttributeBehavior name w cache attr = nativeSignal f where
    f s = do
       isLive name >>= (\live -> if live then processLiveWidget s else processDeadWidget)
    processLiveWidget s = checkCurrentCachedValue s >>= maybe (runBehavior s) return 
    processDeadWidget = getLastCachedValue cache attr >>= maybe deadWidgetError return
    checkCurrentCachedValue (Stimulus t) = do
        CachedProperty cv _ <- safeFetchCacheProperty
        let cacheValid = maybe False ((== t) . fst) cv
        if (cacheValid) 
            then {- traceAction "Using cache: " >> -} (return . cast . snd . fromJust $ cv) 
            else return Nothing
    runBehavior s = do
        CachedProperty _ beh <- safeFetchCacheProperty
        maybe (defaultBehavior s) (checkAndExecuteBehavior s) beh
    checkAndExecuteBehavior s beh@(SignalC _) = executeBehavior s beh
    checkAndExecuteBehavior s beh@(ConstSignalC v) = do 
      isNew <- isCurrentUpdateOp name (WX.attrName attr)
      if isNew then executeBehavior s beh else {-traceAction "Skipping update on " >>-} return v
    executeBehavior s@(Stimulus t) beh = do
        {-traceAction "Executing behavior: "-}
        (newBeh, value) <- stepSignal s beh
        liftIO $ WX.set w [attr := value]  -- Write back to control
        updateCache cache attr $ CachedProperty (Just (t, value)) (Just newBeh)
        return value
    defaultBehavior (Stimulus t) = do
        {-traceAction "Doing default behavior: "-}
        value <- liftIO $ WX.get w attr
        updateCache cache attr $ CachedProperty (Just (t, value)) Nothing
        return value
    safeFetchCacheProperty = getFromCacheWithDefault cache attr (CachedProperty Nothing Nothing)
    deadWidgetError = do
      let message = printf "Error: Widget %s died before values for attribute %s could be cached" name (WX.attrName attr)
      error message
    {-traceAction str = liftIO . traceIO $ str ++ name ++ " " ++ (WX.attrName attr)-}


------------------------------------------------------------------------------------------------------------

data AnyWXWindow = forall a. AnyWXWindow (WX.Window a)

class Component w where
  nativeComponent :: w -> AnyWXWindow
  getLayout :: w -> WX.Layout

class Container w where
  nativeContainer :: w -> AnyWXWindow 

data AnyContainer = forall w. Container w => AnyContainer w

instance Container AnyContainer  where
  nativeContainer (AnyContainer c) = nativeContainer c

class GraphicsContainer w where  
  nativeGraphicsContainer :: w -> WXExt.AnyGraphicsContainer

data AnyGraphicsContainer = forall w. GraphicsContainer w => AnyGraphicsContainer w

instance GraphicsContainer AnyGraphicsContainer where
  nativeGraphicsContainer (AnyGraphicsContainer c) = nativeGraphicsContainer c
  
deriving instance Typeable WX.Layout  

-------------------------------------------------------------------------------------------------

updateAllAttributes :: Stimulus -> AttributeBehaviorCache -> Druid ()
updateAllAttributes st cc = processAttributeBehaviors cc (process st) where
  process st beh = void $ stepSignal st beh

registerListener :: w -> WX.Event w a -> Druid a -> Druid ()
registerListener w ev handler = handler >>= \f -> liftIO $ WX.set w [WX.on ev := f] >> (traceIO $ "Registered: " ++ (WX.attrName $ WX.on ev))

standardEventReceiver :: WXEvent -> Druid(IO ())
standardEventReceiver ev = f <$> drDataRef <*> qRef <*> inPRef
  where
    drDataRef = getStepperDataRef
    qRef = getEventQueueRef
    inPRef = getInProgressRef
    f ref qRef inPRef = processEvents ev qRef inPRef (stepEngineIORef ref) {-<* (liftIO . traceIO) ("Done processing events")-}

standardEventReceiver1 :: (a -> WXEvent) -> Druid(a -> IO ())
standardEventReceiver1 evCreator = f <$> drDataRef <*> qRef <*> inPRef
  where
    drDataRef = getStepperDataRef
    qRef = getEventQueueRef
    inPRef = getInProgressRef
    f ref qRef inPRef arg = processEvents (evCreator arg) qRef inPRef (stepEngineIORef ref) {-<* (liftIO . traceIO) ("Done processing events")-}

getCurrentBehaviorValue :: Typeable a => w -> PropertyCache -> WX.Attr w a -> Druid a
getCurrentBehaviorValue w cc attr = do
  prop <- getFromCache cc attr
  let cv = hasCachedValue prop
  if (isJust cv) then return $ fromJust cv else liftIO $ WX.get w attr
  where
    hasCachedValue (Just (CachedProperty (Just (_,v)) _)) = Just v
    hasCachedValue _ = Nothing

-------------------------------------------------------------------------------------------------

registerWindow :: ReactiveWidget a => a -> Druid a
registerWindow control = addReactive control >> setLiveness (getName control) True >> return control

unregisterWindow :: ReactiveWidget a => a -> Druid ()
unregisterWindow control = setLiveness (getName control) False

getNativeId :: WX.Identity w => w -> Druid Int
getNativeId w = liftIO $ WX.get w WX.identity

genId :: w -> Druid Int
genId _ = getNextId

createStandardControl :: (Container c, ReactiveProxy p, ReactiveWidget p) => c -> (forall a. WX.Window a -> [WX.Prop w] -> IO w) -> (w -> Int -> AttributeBehaviorCache -> PropertyCache -> p) -> (w -> Druid Int) -> Druid p
createStandardControl container wxconstructor proxyconstructor idgen = do
  AnyWXWindow parent <- return $ nativeContainer container
  w <- liftIO $ wxconstructor parent []
  wid <- idgen w
  control <- proxyconstructor w wid <$> createAttributeBehaviorCache <*> createPropertyCache
  registerWindow control

createGraphicsControl :: (GraphicsContainer c, WXExt.GraphicsComponent w, ReactiveProxy p, ReactiveWidget p) => c -> (forall a. WXExt.GraphicsContainer a => a -> [WX.Prop w] -> IO w) -> (w -> Int -> AttributeBehaviorCache -> PropertyCache -> p) -> (w -> Druid Int) -> Druid p
createGraphicsControl container wxconstructor proxyconstructor idgen = do
  let graphicsContainer = nativeGraphicsContainer container
  graphics <- liftIO $ wxconstructor graphicsContainer []
  liftIO $ WXExt.addGraphics graphicsContainer graphics
  wid <- idgen graphics
  proxy <- proxyconstructor graphics wid <$> createAttributeBehaviorCache <*> createPropertyCache
  registerWindow proxy
  

-------------------------------------------------------------------------------------------------

addEvent :: IORef [WXEvent] -> WXEvent -> IO ()
addEvent qRef ev = modifyIORef' qRef (++ [ev])

popEvent :: IORef [WXEvent] -> IO ()
popEvent qRef = modifyIORef' qRef tail {- >> readIORef qRef >>= return . length >>= traceIO . ("Queue Size: " ++) . show-}

hasPendingEvents :: IORef [WXEvent] -> IO Bool
hasPendingEvents qRef = readIORef qRef >>= return . not . null

processEvents :: WXEvent -> IORef [WXEvent] -> IORef Bool -> IO () -> IO ()
processEvents ev qRef inProgressRef action = do
  addEvent qRef ev
  isProcessing <- readIORef inProgressRef
  if isProcessing 
    then   {-traceEventAction "Queueing Event and returning:" >> -}  return ()  
    else   {-traceEventAction "About to process " >> -}  continueProcessing
  where
    continueProcessing = do
      action
      popEvent qRef
      pending <- hasPendingEvents qRef
      {-Control.Monad.when pending $ traceIO "Continuing processing"-}
      if pending then continueProcessing else return ()
    traceEventAction msg = traceIO $ msg ++ " " ++ (show ev)
      
      

fromWXEvent :: (WXEvent -> Maybe v) -> Event v
fromWXEvent fn = nativeEvent f where
  f _ = getLatestEvent >>= return . (maybe Nothing fn)

-------------------------------------- PROXIES AND EXTRAS --------------------------------

textChange :: WX.Event (WXCore.Control a) (IO ())
textChange = WX.newEvent "textChange" (WXCore.controlGetOnText) (WXCore.controlOnText)

--------------------------------------------------------------------------------

class CommandEventSource w where
  onCommand :: w -> Druid (Event ())

class TextChangeEventSource w where
  onTextChange :: w -> Druid (Event())

class SelectEventSource w where
  onSelect :: w -> Druid (Event())

class ReactiveEventSource w where
  onMouse :: w -> Druid (Event WX.EventMouse)
  onKeyboard :: w -> Druid (Event WX.EventKey)
  onFocus :: w -> Druid (Event Bool)
  onActivate :: w -> Druid (Event Bool)
  onResize :: w -> Druid (Event ())
  onClosing :: w -> Druid (Event ())
  onEnter :: w -> Druid (Event WX.Point)
  onLeave :: w -> Druid (Event WX.Point)
  onMotion :: w -> Druid (Event WX.Point)
  onDrag :: w -> Druid (Event WX.Point)
  onClick :: w -> Druid (Event WX.Point)
  onUnclick :: w -> Druid (Event WX.Point)
  onDoubleClick :: w -> Druid (Event WX.Point)
  onClickRight :: w -> Druid (Event WX.Point)
  onUnclickRight :: w -> Druid (Event WX.Point)
  onKey :: w -> WX.Key -> Druid (Event ())


instance (ReactiveProxy w, WX.Commanding (Delegate w)) => CommandEventSource w where
  onCommand w =  registerHandler >> return (fromWXEvent eventFilter) where
    registerHandler = registerListener (getDelegate w) WX.command eventReactor
    eventReactor = standardEventReceiver $ WXCommand (getName w)
    eventFilter (WXCommand id') | (getName w) == id' = Just ()
    eventFilter _ = Nothing

instance (ReactiveProxy w, WX.Selecting (Delegate w)) => SelectEventSource w where
  onSelect w =  registerHandler >> return (fromWXEvent eventFilter) where
    registerHandler = registerListener (getDelegate w) WX.select eventReactor
    eventReactor = standardEventReceiver $ WXSelect (getName w)
    eventFilter (WXSelect id') | (getName w) == id' = Just ()
    eventFilter _ = Nothing

instance (ReactiveProxy w, WX.Reactive (Delegate w)) => ReactiveEventSource w where
  onMouse w =  registerHandler >> return (fromWXEvent eventFilter) where
    registerHandler = registerListener (getDelegate w) WX.mouse eventReactor
    eventReactor = standardEventReceiver1 $ WXMouse (getName w)
    eventFilter (WXMouse id' ev) | (getName w) == id' = Just ev
    eventFilter _ = Nothing
  onKeyboard w =  registerHandler >> return (fromWXEvent eventFilter) where
    registerHandler = registerListener (getDelegate w) WX.keyboard eventReactor
    eventReactor = standardEventReceiver1 $ WXKeyboard (getName w)
    eventFilter (WXKeyboard id' ev) | (getName w) == id' = Just ev
    eventFilter _ = Nothing
  onFocus w =  registerHandler >> return (fromWXEvent eventFilter) where
    registerHandler = registerListener (getDelegate w) WX.focus eventReactor
    eventReactor = standardEventReceiver1 $ WXFocus (getName w)
    eventFilter (WXFocus id' focused) | (getName w) == id' = Just focused
    eventFilter _ = Nothing
  onActivate w =  registerHandler >> return (fromWXEvent eventFilter) where
    registerHandler = registerListener (getDelegate w) WX.activate eventReactor
    eventReactor = standardEventReceiver1 $ WXActivate (getName w)
    eventFilter (WXActivate id' activated) | (getName w) == id' = Just activated
    eventFilter _ = Nothing
  onClosing w =  registerHandler >> return (fromWXEvent eventFilter) where
    registerHandler = registerListener (getDelegate w) WX.closing eventReactor
    eventReactor = standardEventReceiver $ WXClosing (getName w)
    eventFilter (WXClosing id') | (getName w) == id' = Just ()
    eventFilter _ = Nothing
  onResize w =  registerHandler >> return (fromWXEvent eventFilter) where
    registerHandler = registerListener (getDelegate w) WX.resize eventReactor
    eventReactor = standardEventReceiver $ WXResize (getName w)
    eventFilter (WXResize id') | (getName w) == id' = Just ()
    eventFilter _ = Nothing
  onEnter w =  registerHandler >> return (fromWXEvent eventFilter) where
    registerHandler = registerListener (getDelegate w) WX.enter eventReactor
    eventReactor = standardEventReceiver1 $ WXEnter (getName w)
    eventFilter (WXEnter id' pt) | (getName w) == id' = Just pt
    eventFilter _ = Nothing
  onLeave w =  registerHandler >> return (fromWXEvent eventFilter) where
    registerHandler = registerListener (getDelegate w) WX.leave eventReactor
    eventReactor = standardEventReceiver1 $ WXLeave (getName w)
    eventFilter (WXLeave id' pt) | (getName w) == id' = Just pt
    eventFilter _ = Nothing
  onMotion w =  registerHandler >> return (fromWXEvent eventFilter) where
    registerHandler = registerListener (getDelegate w) WX.motion eventReactor
    eventReactor = standardEventReceiver1 $ WXMotion (getName w)
    eventFilter (WXMotion id' pt) | (getName w) == id' = Just pt
    eventFilter _ = Nothing
  onDrag w =  registerHandler >> return (fromWXEvent eventFilter) where
    registerHandler = registerListener (getDelegate w) WX.drag eventReactor
    eventReactor = standardEventReceiver1 $ WXDrag (getName w)
    eventFilter (WXDrag id' pt) | (getName w) == id' = Just pt
    eventFilter _ = Nothing
  onClick w =  registerHandler >> return (fromWXEvent eventFilter) where
    registerHandler = registerListener (getDelegate w) WX.click eventReactor
    eventReactor = standardEventReceiver1 $ WXClick (getName w)
    eventFilter (WXClick id' pt) | (getName w) == id' = Just pt
    eventFilter _ = Nothing
  onUnclick w =  registerHandler >> return (fromWXEvent eventFilter) where
    registerHandler = registerListener (getDelegate w) WX.unclick eventReactor
    eventReactor = standardEventReceiver1 $ WXUnclick (getName w)
    eventFilter (WXUnclick id' pt) | (getName w) == id' = Just pt
    eventFilter _ = Nothing
  onDoubleClick w =  registerHandler >> return (fromWXEvent eventFilter) where
    registerHandler = registerListener (getDelegate w) WX.doubleClick eventReactor
    eventReactor = standardEventReceiver1 $ WXDoubleClick (getName w)
    eventFilter (WXDoubleClick id' pt) | (getName w) == id' = Just pt
    eventFilter _ = Nothing
  onClickRight w =  registerHandler >> return (fromWXEvent eventFilter) where
    registerHandler = registerListener (getDelegate w) WX.clickRight eventReactor
    eventReactor = standardEventReceiver1 $ WXClickRight (getName w)
    eventFilter (WXClickRight id' pt) | (getName w) == id' = Just pt
    eventFilter _ = Nothing
  onUnclickRight w =  registerHandler >> return (fromWXEvent eventFilter) where
    registerHandler = registerListener (getDelegate w) WX.unclickRight eventReactor
    eventReactor = standardEventReceiver1 $ WXUnclickRight (getName w)
    eventFilter (WXUnclickRight id' pt) | (getName w) == id' = Just pt
    eventFilter _ = Nothing
  onKey w key =  registerHandler >> (return . trace) ("On Key " ++ (show key)) >> return (fromWXEvent eventFilter) where
    registerHandler = registerListener (getDelegate w) (WX.key key) eventReactor
    eventReactor = (return . trace) ("Registered for " ++ (getName w) ++ " " ++ (show key)) >> standardEventReceiver $ WXKey (getName w) key
    eventFilter (WXKey id' key') | (getName w) == id' && key' == key = Just ()
    eventFilter _ = Nothing


--------------------------------------------------------------------------------

data WXFrame = WXFrame (WX.Frame ()) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXFrame where
  type Delegate WXFrame = WX.Frame ()
  type Attribute WXFrame = WX.Attr (WX.Frame ())
  type CreateAttr WXFrame =   (Bool, Bool, Bool, Bool)
  type ReactorMonad WXFrame = Druid
  getDelegate (WXFrame w _ _ _) = w
  getName (WXFrame _ wid _ _) = "WXFrame" ++ (show wid)
  getAttr c@(WXFrame w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXFrame w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr)
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXFrame w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create (bResizeable, bMinimizeable, bMaximizeable, bCloseable) = do
    w <- liftIO $ WXExt.createFrame [WX.resizeable := bResizeable, WX.minimizeable := bMinimizeable, WX.maximizeable := bMaximizeable, WX.closeable := bCloseable]
    wid <- liftIO $ WX.get w WX.identity
    attrCache <- liftIO $ H.new
    connCache <- liftIO $ H.new
    registerWindow $ WXFrame w wid attrCache connCache
  remove c@(WXFrame w _ _ _) = liftIO (void $ WXC.windowClose w False) >> unregisterWindow c
  
instance ReactiveWidget WXFrame where
  updateAttributes st (WXFrame _ _ cc _) = updateAllAttributes st cc  

createWXFrame :: [AttributeBehavior WXFrame] -> Druid WXFrame
createWXFrame props = do
    window <- create (True, True, True, True)
    setAttrs window props
    return window
    
createWXFrameFixed :: [AttributeBehavior WXFrame] -> Druid WXFrame
createWXFrameFixed props = do
    window <- create (False, True, False, True)
    setAttrs window props
    return window

instance Component WXFrame where
  nativeComponent (WXFrame w _ _ _) = AnyWXWindow w
  getLayout (WXFrame w _ _ _) = WX.widget w

instance Container WXFrame where
  nativeContainer (WXFrame w _ _ _) = AnyWXWindow w

instance GraphicsContainer WXFrame where
  nativeGraphicsContainer (WXFrame w _ _ _) = WXExt.AnyGraphicsContainer w


--------------------------------------------------------------------------------


data WXTimer = WXTimer (WX.Timer) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXTimer where
  type Delegate WXTimer = WX.Timer
  type Attribute WXTimer = WX.Attr (WX.Timer)
  type CreateAttr WXTimer = AnyContainer
  type ReactorMonad WXTimer = Druid
  getDelegate (WXTimer w _ _ _) = w
  getName (WXTimer _ wid _ _) = "WXTimer" ++ (show wid)
  getAttr c@(WXTimer w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXTimer w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr)
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXTimer w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createStandardControl parent WX.timer WXTimer genId
  remove c@(WXTimer w _ _ _) = liftIO (WXC.timerDelete w) >> unregisterWindow c

  
instance ReactiveWidget WXTimer where
  updateAttributes st (WXTimer _ _ cc _) = updateAllAttributes st cc  

createWXTimer :: Container c => c -> [AttributeBehavior WXTimer] -> Druid WXTimer
createWXTimer parent props = do
  control <- create (AnyContainer parent)
  setAttrs control props
  return control


--------------------------------------------------------------------------------


data WXButton = WXButton (WX.Button ()) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXButton where
  type Delegate WXButton = WX.Button ()
  type Attribute WXButton = WX.Attr (WX.Button ())
  type CreateAttr WXButton = AnyContainer
  type ReactorMonad WXButton = Druid
  getDelegate (WXButton w _ _ _) = w
  getName (WXButton _ wid _ _) = "WXButton" ++ (show wid)
  getAttr c@(WXButton w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXButton w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr)
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXButton w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createStandardControl parent WX.button WXButton getNativeId
  remove c@(WXButton w _ _ _) = liftIO (WXC.windowDestroy w) >> unregisterWindow c

  
instance ReactiveWidget WXButton where
  updateAttributes st (WXButton _ _ cc _) = updateAllAttributes st cc  

createWXButton :: Container c => c -> [AttributeBehavior WXButton] -> Druid WXButton
createWXButton parent props = do
  control <- create (AnyContainer parent)
  setAttrs control props
  return control

instance Component WXButton where
  nativeComponent (WXButton w _ _ _) = AnyWXWindow w
  getLayout (WXButton w _ _ _) = WX.widget w


--------------------------------------------------------------------------------


data WXToggleButton = WXToggleButton (WX.ToggleButton ()) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXToggleButton where
  type Delegate WXToggleButton = WX.ToggleButton ()
  type Attribute WXToggleButton = WX.Attr (WX.ToggleButton ())
  type CreateAttr WXToggleButton = AnyContainer
  type ReactorMonad WXToggleButton = Druid
  getDelegate (WXToggleButton w _ _ _) = w
  getName (WXToggleButton _ wid _ _) = "WXToggleButton" ++ (show wid)
  getAttr c@(WXToggleButton w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXToggleButton w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr)
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXToggleButton w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createStandardControl parent WX.toggleButton WXToggleButton getNativeId
  remove c@(WXToggleButton w _ _ _) = liftIO (WXC.windowDestroy w) >> unregisterWindow c

  
instance ReactiveWidget WXToggleButton where
  updateAttributes st (WXToggleButton _ _ cc _) = updateAllAttributes st cc  

createWXToggleButton :: Container c => c -> [AttributeBehavior WXToggleButton] -> Druid WXToggleButton
createWXToggleButton parent props = do
  control <- create (AnyContainer parent)
  setAttrs control props
  return control

instance Component WXToggleButton where
  nativeComponent (WXToggleButton w _ _ _) = AnyWXWindow w
  getLayout (WXToggleButton w _ _ _) = WX.widget w


--------------------------------------------------------------------------------


data WXBitmapToggleButton = WXBitmapToggleButton (WX.BitmapToggleButton ()) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXBitmapToggleButton where
  type Delegate WXBitmapToggleButton = WX.BitmapToggleButton ()
  type Attribute WXBitmapToggleButton = WX.Attr (WX.BitmapToggleButton ())
  type CreateAttr WXBitmapToggleButton = AnyContainer
  type ReactorMonad WXBitmapToggleButton = Druid
  getDelegate (WXBitmapToggleButton w _ _ _) = w
  getName (WXBitmapToggleButton _ wid _ _) = "WXBitmapToggleButton" ++ (show wid)
  getAttr c@(WXBitmapToggleButton w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXBitmapToggleButton w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr)
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXBitmapToggleButton w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createStandardControl parent WX.bitmapToggleButton WXBitmapToggleButton getNativeId
  remove c@(WXBitmapToggleButton w _ _ _) = liftIO (WXC.windowDestroy w) >> unregisterWindow c

  
instance ReactiveWidget WXBitmapToggleButton where
  updateAttributes st (WXBitmapToggleButton _ _ cc _) = updateAllAttributes st cc  

createWXBitmapToggleButton :: Container c => c -> [AttributeBehavior WXBitmapToggleButton] -> Druid WXBitmapToggleButton
createWXBitmapToggleButton parent props = do
  control <- create (AnyContainer parent)
  setAttrs control props
  return control

instance Component WXBitmapToggleButton where
  nativeComponent (WXBitmapToggleButton w _ _ _) = AnyWXWindow w
  getLayout (WXBitmapToggleButton w _ _ _) = WX.widget w


--------------------------------------------------------------------------------


data WXLabel = WXLabel (WX.StaticText ()) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXLabel where
  type Delegate WXLabel = WX.StaticText ()
  type Attribute WXLabel = WX.Attr (WX.StaticText ())
  type CreateAttr WXLabel = AnyContainer
  type ReactorMonad WXLabel = Druid
  getDelegate (WXLabel w _ _ _) = w
  getName (WXLabel _ wid _ _) = "WXLabel" ++ (show wid)
  getAttr c@(WXLabel w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXLabel w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr)
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXLabel w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createStandardControl parent WX.staticText WXLabel getNativeId
  remove c@(WXLabel w _ _ _) = liftIO (WXC.windowDestroy w) >> unregisterWindow c

  
instance ReactiveWidget WXLabel where
  updateAttributes st (WXLabel _ _ cc _) = updateAllAttributes st cc  

createWXLabel :: Container c => c -> [AttributeBehavior WXLabel] -> Druid WXLabel
createWXLabel parent props = do
  control <- create (AnyContainer parent)
  setAttrs control props
  return control

instance Component WXLabel where
  nativeComponent (WXLabel w _ _ _) = AnyWXWindow w
  getLayout (WXLabel w _ _ _) = WX.widget w


--------------------------------------------------------------------------------


data WXTextEntry = WXTextEntry (WX.TextCtrl ()) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXTextEntry where
  type Delegate WXTextEntry = WX.TextCtrl ()
  type Attribute WXTextEntry = WX.Attr (WX.TextCtrl ())
  type CreateAttr WXTextEntry = AnyContainer
  type ReactorMonad WXTextEntry = Druid
  getDelegate (WXTextEntry w _ _ _) = w
  getName (WXTextEntry _ wid _ _) = "WXTextEntry" ++ (show wid)
  getAttr c@(WXTextEntry w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXTextEntry w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr)
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXTextEntry w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createStandardControl parent WX.textEntry WXTextEntry getNativeId
  remove c@(WXTextEntry w _ _ _) = liftIO (WXC.windowDestroy w) >> unregisterWindow c

  
instance ReactiveWidget WXTextEntry where
  updateAttributes st (WXTextEntry _ _ cc _) = updateAllAttributes st cc  

createWXTextEntry :: Container c => c -> [AttributeBehavior WXTextEntry] -> Druid WXTextEntry
createWXTextEntry parent props = do
  control <- create (AnyContainer parent)
  setAttrs control props
  return control

instance TextChangeEventSource WXTextEntry where
  onTextChange w = registerHandler >> return (fromWXEvent eventFilter)
    where
      registerHandler = registerListener (getDelegate w) textChange eventReactor
      eventReactor = standardEventReceiver . WXTextChange $ getName w
      eventFilter (WXTextChange id') | (getName w) == id' = Just ()
      eventFilter _ = Nothing

instance Component WXTextEntry where
  nativeComponent (WXTextEntry w _ _ _) = AnyWXWindow w
  getLayout (WXTextEntry w _ _ _) = WX.widget w


--------------------------------------------------------------------------------


data WXCheckBox = WXCheckBox (WX.CheckBox ()) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXCheckBox where
  type Delegate WXCheckBox = WX.CheckBox ()
  type Attribute WXCheckBox = WX.Attr (WX.CheckBox ())
  type CreateAttr WXCheckBox = AnyContainer
  type ReactorMonad WXCheckBox = Druid
  getDelegate (WXCheckBox w _ _ _) = w
  getName (WXCheckBox _ wid _ _) = "WXCheckBox" ++ (show wid)
  getAttr c@(WXCheckBox w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXCheckBox w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr)
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXCheckBox w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createStandardControl parent WX.checkBox WXCheckBox getNativeId
  remove c@(WXCheckBox w _ _ _) = liftIO (WXC.windowDestroy w) >> unregisterWindow c

  
instance ReactiveWidget WXCheckBox where
  updateAttributes st (WXCheckBox _ _ cc _) = updateAllAttributes st cc  

createWXCheckBox :: Container c => c -> [AttributeBehavior WXCheckBox] -> Druid WXCheckBox
createWXCheckBox parent props = do
  control <- create (AnyContainer parent)
  setAttrs control props
  return control

instance Component WXCheckBox where
  nativeComponent (WXCheckBox w _ _ _) = AnyWXWindow w
  getLayout (WXCheckBox w _ _ _) = WX.widget w


--------------------------------------------------------------------------------


data WXChoice = WXChoice (WX.Choice ()) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXChoice where
  type Delegate WXChoice = WX.Choice ()
  type Attribute WXChoice = WX.Attr (WX.Choice ())
  type CreateAttr WXChoice = AnyContainer
  type ReactorMonad WXChoice = Druid
  getDelegate (WXChoice w _ _ _) = w
  getName (WXChoice _ wid _ _) = "WXChoice" ++ (show wid)
  getAttr c@(WXChoice w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXChoice w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr)
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXChoice w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createStandardControl parent WX.choice WXChoice getNativeId
  remove c@(WXChoice w _ _ _) = liftIO (WXC.windowDestroy w) >> unregisterWindow c

  
instance ReactiveWidget WXChoice where
  updateAttributes st (WXChoice _ _ cc _) = updateAllAttributes st cc  

createWXChoice :: Container c => c -> [AttributeBehavior WXChoice] -> Druid WXChoice
createWXChoice parent props = do
  control <- create (AnyContainer parent)
  setAttrs control props
  return control

instance Component WXChoice where
  nativeComponent (WXChoice w _ _ _) = AnyWXWindow w
  getLayout (WXChoice w _ _ _) = WX.widget w


--------------------------------------------------------------------------------


data WXComboBox = WXComboBox (WX.ComboBox ()) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXComboBox where
  type Delegate WXComboBox = WX.ComboBox ()
  type Attribute WXComboBox = WX.Attr (WX.ComboBox ())
  type CreateAttr WXComboBox = AnyContainer
  type ReactorMonad WXComboBox = Druid
  getDelegate (WXComboBox w _ _ _) = w
  getName (WXComboBox _ wid _ _) = "WXComboBox" ++ (show wid)
  getAttr c@(WXComboBox w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXComboBox w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr)
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXComboBox w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createStandardControl parent WX.comboBox WXComboBox getNativeId
  remove c@(WXComboBox w _ _ _) = liftIO (WXC.windowDestroy w) >> unregisterWindow c

  
instance ReactiveWidget WXComboBox where
  updateAttributes st (WXComboBox _ _ cc _) = updateAllAttributes st cc  

createWXComboBox :: Container c => c -> [AttributeBehavior WXComboBox] -> Druid WXComboBox
createWXComboBox parent props = do
  control <- create (AnyContainer parent)
  setAttrs control props
  return control

instance Component WXComboBox where
  nativeComponent (WXComboBox w _ _ _) = AnyWXWindow w
  getLayout (WXComboBox w _ _ _) = WX.widget w


--------------------------------------------------------------------------------


data WXListBox = WXListBox (WX.SingleListBox ()) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXListBox where
  type Delegate WXListBox = WX.SingleListBox ()
  type Attribute WXListBox = WX.Attr (WX.SingleListBox ())
  type CreateAttr WXListBox = AnyContainer
  type ReactorMonad WXListBox = Druid
  getDelegate (WXListBox w _ _ _) = w
  getName (WXListBox _ wid _ _) = "WXListBox" ++ (show wid)
  getAttr c@(WXListBox w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXListBox w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr)
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXListBox w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createStandardControl parent WX.singleListBox WXListBox getNativeId
  remove c@(WXListBox w _ _ _) = liftIO (WXC.windowDestroy w) >> unregisterWindow c

  
instance ReactiveWidget WXListBox where
  updateAttributes st (WXListBox _ _ cc _) = updateAllAttributes st cc  

createWXListBox :: Container c => c -> [AttributeBehavior WXListBox] -> Druid WXListBox
createWXListBox parent props = do
  control <- create (AnyContainer parent)
  setAttrs control props
  return control

instance Component WXListBox where
  nativeComponent (WXListBox w _ _ _) = AnyWXWindow w
  getLayout (WXListBox w _ _ _) = WX.widget w


--------------------------------------------------------------------------------


data WXMultiListBox = WXMultiListBox (WX.MultiListBox ()) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXMultiListBox where
  type Delegate WXMultiListBox = WX.MultiListBox ()
  type Attribute WXMultiListBox = WX.Attr (WX.MultiListBox ())
  type CreateAttr WXMultiListBox = AnyContainer
  type ReactorMonad WXMultiListBox = Druid
  getDelegate (WXMultiListBox w _ _ _) = w
  getName (WXMultiListBox _ wid _ _) = "WXMultiListBox" ++ (show wid)
  getAttr c@(WXMultiListBox w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXMultiListBox w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr)
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXMultiListBox w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createStandardControl parent WX.multiListBox WXMultiListBox getNativeId
  remove c@(WXMultiListBox w _ _ _) = liftIO (WXC.windowDestroy w) >> unregisterWindow c

  
instance ReactiveWidget WXMultiListBox where
  updateAttributes st (WXMultiListBox _ _ cc _) = updateAllAttributes st cc  

createWXMultiListBox :: Container c => c -> [AttributeBehavior WXMultiListBox] -> Druid WXMultiListBox
createWXMultiListBox parent props = do
  control <- create (AnyContainer parent)
  setAttrs control props
  return control

instance Component WXMultiListBox where
  nativeComponent (WXMultiListBox w _ _ _) = AnyWXWindow w
  getLayout (WXMultiListBox w _ _ _) = WX.widget w


--------------------------------------------------------------------------------


data WXTreeControl = WXTreeControl (WX.TreeCtrl ()) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXTreeControl where
  type Delegate WXTreeControl = WX.TreeCtrl ()
  type Attribute WXTreeControl = WX.Attr (WX.TreeCtrl ())
  type CreateAttr WXTreeControl = AnyContainer
  type ReactorMonad WXTreeControl = Druid
  getDelegate (WXTreeControl w _ _ _) = w
  getName (WXTreeControl _ wid _ _) = "WXTreeControl" ++ (show wid)
  getAttr c@(WXTreeControl w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXTreeControl w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr)
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXTreeControl w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createStandardControl parent WX.treeCtrl WXTreeControl getNativeId
  remove c@(WXTreeControl w _ _ _) = liftIO (WXC.windowDestroy w) >> unregisterWindow c

  
instance ReactiveWidget WXTreeControl where
  updateAttributes st (WXTreeControl _ _ cc _) = updateAllAttributes st cc  

createWXTreeControl :: Container c => c -> [AttributeBehavior WXTreeControl] -> Druid WXTreeControl
createWXTreeControl parent props = do
  control <- create (AnyContainer parent)
  setAttrs control props
  return control

instance Component WXTreeControl where
  nativeComponent (WXTreeControl w _ _ _) = AnyWXWindow w
  getLayout (WXTreeControl w _ _ _) = WX.widget w


--------------------------------------------------------------------------------


data WXListControl = WXListControl (WX.ListCtrl ()) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXListControl where
  type Delegate WXListControl = WX.ListCtrl ()
  type Attribute WXListControl = WX.Attr (WX.ListCtrl ())
  type CreateAttr WXListControl = AnyContainer
  type ReactorMonad WXListControl = Druid
  getDelegate (WXListControl w _ _ _) = w
  getName (WXListControl _ wid _ _) = "WXListControl" ++ (show wid)
  getAttr c@(WXListControl w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXListControl w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr)
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXListControl w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createStandardControl parent WX.listCtrl WXListControl getNativeId
  remove c@(WXListControl w _ _ _) = liftIO (WXC.windowDestroy w) >> unregisterWindow c

  
instance ReactiveWidget WXListControl where
  updateAttributes st (WXListControl _ _ cc _) = updateAllAttributes st cc  

createWXListControl :: Container c => c -> [AttributeBehavior WXListControl] -> Druid WXListControl
createWXListControl parent props = do
  control <- create (AnyContainer parent)
  setAttrs control props
  return control

instance Component WXListControl where
  nativeComponent (WXListControl w _ _ _) = AnyWXWindow w
  getLayout (WXListControl w _ _ _) = WX.widget w


--------------------------------------------------------------------------------


data WXSplitterWindow = WXSplitterWindow (WX.SplitterWindow ()) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXSplitterWindow where
  type Delegate WXSplitterWindow = WX.SplitterWindow ()
  type Attribute WXSplitterWindow = WX.Attr (WX.SplitterWindow ())
  type CreateAttr WXSplitterWindow = AnyContainer
  type ReactorMonad WXSplitterWindow = Druid
  getDelegate (WXSplitterWindow w _ _ _) = w
  getName (WXSplitterWindow _ wid _ _) = "WXSplitterWindow" ++ (show wid)
  getAttr c@(WXSplitterWindow w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXSplitterWindow w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr)
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXSplitterWindow w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createStandardControl parent WX.splitterWindow WXSplitterWindow getNativeId
  remove c@(WXSplitterWindow w _ _ _) = liftIO (WXC.windowDestroy w) >> unregisterWindow c

  
instance ReactiveWidget WXSplitterWindow where
  updateAttributes st (WXSplitterWindow _ _ cc _) = updateAllAttributes st cc  

createWXSplitterWindow :: Container c => c -> [AttributeBehavior WXSplitterWindow] -> Druid WXSplitterWindow
createWXSplitterWindow parent props = do
  control <- create (AnyContainer parent)
  setAttrs control props
  return control

instance Component WXSplitterWindow where
  nativeComponent (WXSplitterWindow w _ _ _) = AnyWXWindow w
  getLayout (WXSplitterWindow w _ _ _) = WX.widget w


--------------------------------------------------------------------------------


data WXMediaControl = WXMediaControl (WX.MediaCtrl ()) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXMediaControl where
  type Delegate WXMediaControl = WX.MediaCtrl ()
  type Attribute WXMediaControl = WX.Attr (WX.MediaCtrl ())
  type CreateAttr WXMediaControl = AnyContainer
  type ReactorMonad WXMediaControl = Druid
  getDelegate (WXMediaControl w _ _ _) = w
  getName (WXMediaControl _ wid _ _) = "WXMediaControl" ++ (show wid)
  getAttr c@(WXMediaControl w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXMediaControl w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr)
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXMediaControl w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createStandardControl parent WX.mediaCtrl WXMediaControl getNativeId
  remove c@(WXMediaControl w _ _ _) = liftIO (WXC.windowDestroy w) >> unregisterWindow c

  
instance ReactiveWidget WXMediaControl where
  updateAttributes st (WXMediaControl _ _ cc _) = updateAllAttributes st cc  

createWXMediaControl :: Container c => c -> [AttributeBehavior WXMediaControl] -> Druid WXMediaControl
createWXMediaControl parent props = do
  control <- create (AnyContainer parent)
  setAttrs control props
  return control

instance Component WXMediaControl where
  nativeComponent (WXMediaControl w _ _ _) = AnyWXWindow w
  getLayout (WXMediaControl w _ _ _) = WX.widget w


--------------------------------------------------------------------------------


data WXStyledTextControl = WXStyledTextControl (WX.StyledTextCtrl ()) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXStyledTextControl where
  type Delegate WXStyledTextControl = WX.StyledTextCtrl ()
  type Attribute WXStyledTextControl = WX.Attr (WX.StyledTextCtrl ())
  type CreateAttr WXStyledTextControl = AnyContainer
  type ReactorMonad WXStyledTextControl = Druid
  getDelegate (WXStyledTextControl w _ _ _) = w
  getName (WXStyledTextControl _ wid _ _) = "WXStyledTextControl" ++ (show wid)
  getAttr c@(WXStyledTextControl w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXStyledTextControl w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr)
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXStyledTextControl w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createStandardControl parent WX.styledTextCtrl WXStyledTextControl getNativeId
  remove c@(WXStyledTextControl w _ _ _) = liftIO (WXC.windowDestroy w) >> unregisterWindow c

  
instance ReactiveWidget WXStyledTextControl where
  updateAttributes st (WXStyledTextControl _ _ cc _) = updateAllAttributes st cc  

createWXStyledTextControl :: Container c => c -> [AttributeBehavior WXStyledTextControl] -> Druid WXStyledTextControl
createWXStyledTextControl parent props = do
  control <- create (AnyContainer parent)
  setAttrs control props
  return control

instance Component WXStyledTextControl where
  nativeComponent (WXStyledTextControl w _ _ _) = AnyWXWindow w
  getLayout (WXStyledTextControl w _ _ _) = WX.widget w


--------------------------------------------------------------------------------


data WXPanel = WXPanel (WX.Panel ()) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXPanel where
  type Delegate WXPanel = WX.Panel ()
  type Attribute WXPanel = WX.Attr (WX.Panel ())
  type CreateAttr WXPanel = AnyContainer
  type ReactorMonad WXPanel = Druid
  getDelegate (WXPanel w _ _ _) = w
  getName (WXPanel _ wid _ _) = "WXPanel" ++ (show wid)
  getAttr c@(WXPanel w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXPanel w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr)
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXPanel w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createStandardControl parent WXExt.createPanel WXPanel getNativeId
  remove c@(WXPanel w _ _ _) = liftIO (WXC.windowDestroy w) >> unregisterWindow c

  
instance ReactiveWidget WXPanel where
  updateAttributes st (WXPanel _ _ cc _) = updateAllAttributes st cc  

createWXPanel :: Container c => c -> [AttributeBehavior WXPanel] -> Druid WXPanel
createWXPanel parent props = do
  control <- create (AnyContainer parent)
  setAttrs control props
  return control

instance Component WXPanel where
  nativeComponent (WXPanel w _ _ _) = AnyWXWindow w
  getLayout (WXPanel w _ _ _) = WX.widget w

instance Container WXPanel where
  nativeContainer (WXPanel w _ _ _) = AnyWXWindow w

instance GraphicsContainer WXPanel where
  nativeGraphicsContainer (WXPanel w _ _ _) = WXExt.AnyGraphicsContainer w


--------------------------------------------------------------------------------


data WXRectangle = WXRectangle (WXExt.Rectangle) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXRectangle where
  type Delegate WXRectangle = WXExt.Rectangle
  type Attribute WXRectangle = WX.Attr (WXExt.Rectangle)
  type CreateAttr WXRectangle = AnyGraphicsContainer
  type ReactorMonad WXRectangle = Druid
  getDelegate (WXRectangle w _ _ _) = w
  getName (WXRectangle _ wid _ _) = "WXRectangle" ++ (show wid)
  getAttr c@(WXRectangle w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXRectangle w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr) -- >> (liftIO . WXExt.requestParentRepaint) w
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXRectangle w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createGraphicsControl parent WXExt.createRectangle WXRectangle getNativeId
  remove c@(WXRectangle w _ _ _) = liftIO (WXExt.removeSimpleGraphics w) >> unregisterWindow c

  
instance ReactiveWidget WXRectangle where
  updateAttributes st (WXRectangle _ _ cc _) = updateAllAttributes st cc  

createWXRectangle :: GraphicsContainer c => c -> [AttributeBehavior WXRectangle] -> Druid WXRectangle
createWXRectangle parent props = do
  control <- create (AnyGraphicsContainer parent)
  setAttrs control props
  return control


--------------------------------------------------------------------------------


data WXEllipse = WXEllipse (WXExt.Ellipse) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXEllipse where
  type Delegate WXEllipse = WXExt.Ellipse
  type Attribute WXEllipse = WX.Attr (WXExt.Ellipse)
  type CreateAttr WXEllipse = AnyGraphicsContainer
  type ReactorMonad WXEllipse = Druid
  getDelegate (WXEllipse w _ _ _) = w
  getName (WXEllipse _ wid _ _) = "WXEllipse" ++ (show wid)
  getAttr c@(WXEllipse w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXEllipse w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr) -- >> (liftIO . WXExt.requestParentRepaint) w
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXEllipse w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createGraphicsControl parent WXExt.createEllipse WXEllipse getNativeId
  remove c@(WXEllipse w _ _ _) = liftIO (WXExt.removeSimpleGraphics w) >> unregisterWindow c

  
instance ReactiveWidget WXEllipse where
  updateAttributes st (WXEllipse _ _ cc _) = updateAllAttributes st cc  

createWXEllipse :: GraphicsContainer c => c -> [AttributeBehavior WXEllipse] -> Druid WXEllipse
createWXEllipse parent props = do
  control <- create (AnyGraphicsContainer parent)
  setAttrs control props
  return control


--------------------------------------------------------------------------------


data WXImage = WXImage (WXExt.Image) Int AttributeBehaviorCache PropertyCache

instance ReactiveProxy WXImage where
  type Delegate WXImage = WXExt.Image
  type Attribute WXImage = WX.Attr (WXExt.Image)
  type CreateAttr WXImage = AnyGraphicsContainer
  type ReactorMonad WXImage = Druid
  getDelegate (WXImage w _ _ _) = w
  getName (WXImage _ wid _ _) = "WXImage" ++ (show wid)
  getAttr c@(WXImage w _ cc cc') attr = getOrAddBehavior cc attr $ makeAttributeBehavior (getName c) w cc' attr
  setAttr f@(WXImage w _ _ cc') attr beh = deferUpdateOp (getName f) (WX.attrName attr) $ updateAssociatedBehavior cc' attr beh >> void (getAttr f attr) -- >> (liftIO . WXExt.requestParentRepaint) w
  react w ev reactor = addReactors [Reactor ev $ reactor w]
  now c@(WXImage w _ _ cc') attr = getAttr c attr >> getCurrentBehaviorValue w cc' attr
  create parent = createGraphicsControl parent WXExt.createImage WXImage getNativeId
  remove c@(WXImage w _ _ _) = liftIO (WXExt.removeSimpleGraphics w) >> unregisterWindow c

  
instance ReactiveWidget WXImage where
  updateAttributes st (WXImage _ _ cc _) = updateAllAttributes st cc  

createWXImage :: GraphicsContainer c => c -> [AttributeBehavior WXImage] -> Druid WXImage
createWXImage parent props = do
  control <- create (AnyGraphicsContainer parent)
  setAttrs control props
  return control


--------------------------------------------------------------------------------


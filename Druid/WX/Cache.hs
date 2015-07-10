{-# LANGUAGE TypeFamilies, ExistentialQuantification, ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Druid.WX.Cache where

import qualified Data.HashTable.IO as H
import qualified Graphics.UI.WX as WX

import Data.Maybe
import Data.Dynamic
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict

import Druid.WX.DruidMonad

data CachedProperty a = Typeable a => CachedProperty (Maybe (Double, a)) (Maybe (Behavior a))

data AnyCachedProperty = forall a. Typeable a => AnyCachedProperty (Maybe (Double, a)) (Maybe (Behavior a))

type PropertyCache = H.CuckooHashTable String AnyCachedProperty

createPropertyCache :: Druid PropertyCache
createPropertyCache = liftIO $ H.new

getFromCache :: Typeable a => PropertyCache -> WX.Attr w a -> Druid (Maybe (CachedProperty a))
getFromCache cache attr =
    liftIO $ H.lookup cache (WX.attrName attr) >>= return . toMaybeCachedProperty
    
toCachedProperty :: Typeable a => AnyCachedProperty -> CachedProperty a
toCachedProperty (AnyCachedProperty cc beh) = CachedProperty (fromJust $ gcast cc) (fromJust $ gcast beh)

toMaybeCachedProperty :: Typeable a => Maybe AnyCachedProperty -> Maybe (CachedProperty a)
toMaybeCachedProperty = maybe Nothing (Just . toCachedProperty) 
    
getFromCacheWithDefault :: Typeable a => PropertyCache -> WX.Attr w a -> CachedProperty a -> Druid (CachedProperty a)
getFromCacheWithDefault cache attr def = getFromCache cache attr >>= maybe (return def) return
    
updateCache :: Typeable a => PropertyCache -> WX.Attr w a -> CachedProperty a -> Druid ()
updateCache cache attr (CachedProperty cp beh) = liftIO $ H.insert cache (WX.attrName attr) (AnyCachedProperty cp beh)

updateAssociatedBehavior :: Typeable a => PropertyCache -> WX.Attr w a -> Behavior a -> Druid ()
updateAssociatedBehavior cache attr beh = do
    updateCache cache attr $ CachedProperty Nothing (Just beh)

getLastCachedValue :: Typeable a => PropertyCache -> WX.Attr w a -> Druid (Maybe a)
getLastCachedValue cache attr = do
  cachedProp <- getFromCache cache attr
  if (isNothing cachedProp) 
    then return Nothing 
    else return . fetchValue $ fromJust cachedProp
  where
  fetchValue (CachedProperty c _)  = maybe Nothing (Just. snd) c


------------------------------------------------------------------------------------------------------------

data AnyAttributeBehavior = forall a. Typeable a => AnyAttributeBehavior (Behavior a)

type AttributeBehaviorCache = H.CuckooHashTable String AnyAttributeBehavior

createAttributeBehaviorCache :: Druid AttributeBehaviorCache
createAttributeBehaviorCache = liftIO $ H.new

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


deriving instance Typeable StateT

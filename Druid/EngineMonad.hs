{-# LANGUAGE TypeFamilies, ConstraintKinds, GADTs, DeriveDataTypeable #-}

module Druid.EngineMonad where

import Control.Monad.IO.Class
import Data.Typeable

------------------------------------------------------------------
-- Data Types for the Engine
------------------------------------------------------------------

data Stimulus = Stimulus Double
  deriving Show

data SignalC m a where
  SignalC :: Monad m => (Stimulus -> m (SignalC m a, a)) -> SignalC m a
  ConstSignalC :: Monad m => a -> SignalC m a
  deriving Typeable

data EventC m a where 
  Event :: Monad m => (Stimulus -> m (EventC m a, Maybe a)) -> EventC m a
  deriving Typeable
             
------------------------------------------------------------------

class Monad m => EngineMonad m where
  updateProxyAttributes :: Stimulus -> m ()
  executeReactors :: Stimulus -> m ()
  performDeferredOperations :: m ()
  finalizeStep :: Stimulus -> m ()

class (MonadIO m, EngineMonad m) => EngineMonadIO m where
  data EngineMonadData m :: *
  runEngineMonadIO :: EngineMonadData m -> m () -> IO (EngineMonadData m)


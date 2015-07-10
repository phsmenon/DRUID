{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Druid.Engine (
  module Druid.Engine,
  module Druid.Combinators
) where 

import Druid.Combinators
import Control.Monad.IO.Class
import Data.Time.Clock

import Druid.EngineMonad

------------------------------------------------------
-- Reactive Loop
------------------------------------------------------

stepEngine :: EngineMonad m => Stimulus -> m ()
stepEngine st = 
  updateProxyAttributes st >> executeReactors st >> performDeferredOperations >> updateProxyAttributes st >> finalizeStep st

stepEngineIO :: EngineMonadIO m => EngineMonadData m -> IO (EngineMonadData m)
stepEngineIO monadData = runEngineMonadIO monadData doStep
  where
    doStep = makeStimulus >>= stepEngine
    makeStimulus = do
      utcTime <- liftIO getCurrentTime
      let tm = realToFrac $ utctDayTime utcTime :: Double
      return $ Stimulus tm


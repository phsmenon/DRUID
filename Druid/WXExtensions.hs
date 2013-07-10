{-# LANGUAGE TypeFamilies, ExistentialQuantification, RankNTypes, ConstraintKinds, FlexibleInstances #-}

module Druid.WXExtensions where

{-import qualified Graphics.UI.WX as WX  hiding ((:=))-}
{-import Graphics.UI.WX(Prop((:=)), Attr(..))-}
import Graphics.UI.WX

import Control.Monad.IO.Class
import Data.IORef

import Druid.DruidMonad

import Debug.Trace


class GraphicsContainer w where
  addGraphics :: GraphicsComponent g => w -> g -> IO ()
  removeGraphics :: GraphicsComponent g => w -> g -> IO ()
  requestRepaint :: w -> IO ()

class GraphicsComponent g where
  onAttachToContainer :: GraphicsContainer w => g -> w -> IO ()
  onDetachFromContainer :: g -> IO ()
  onDraw :: g -> DC () -> IO ()


data AnyGraphicsContainer = forall w. GraphicsContainer w => AnyGraphicsContainer w

data AnyGraphicsComponent = forall w. GraphicsComponent w => AnyGraphicsComponent w

---------------------------------------------------------------------------------

data DimensionData = DimensionData { area :: Rect }

class SimpleGraphics g where
  getParentData :: g -> IORef (Maybe AnyGraphicsContainer)
  getDimensionData :: g -> IORef DimensionData
  draw :: g -> DC () -> IO ()

newtype SimpleGraphicsWrapper a = SimpleGraphicsWrapper { unwrapSimpleGraphics :: a }

instance SimpleGraphics g => GraphicsComponent (SimpleGraphicsWrapper g) where
  onAttachToContainer g w = writeIORef (getParentData $ unwrapSimpleGraphics g) (Just $ AnyGraphicsContainer w)
  onDetachFromContainer g = writeIORef (getParentData $ unwrapSimpleGraphics g) (Nothing)
  onDraw g dc = draw (unwrapSimpleGraphics g) dc

{-instance SimpleGrahics g => Dimensions (SimpleGraphicsWrapper g) where-}
  {-outerSize :: -}

----------------------------------------------------------------------------------





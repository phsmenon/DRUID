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
  onDetachFromContainer :: GraphicsContainer w => g -> w -> IO ()
  onDraw :: g -> DC () -> IO ()


---------------------------------------------------------------------------------

data DimensionData = DimensionData { area :: Rect }


----------------------------------------------------------------------------------

class SimpleGraphics g where
  getParentData :: GraphicsContainer w => g -> IORef (Maybe w)
  getDimensionData :: g -> IORef DimensionData
  draw :: g -> DC () -> IO ()

{-instance (SimpleGraphics a) => GraphicsComponent a where-}
  {-onAttachToContainer g w = return () -}
  {-onDetachFromContainer g w = return ()-}
  {-onDraw g dc = return ()-}



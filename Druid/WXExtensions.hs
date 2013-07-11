{-# LANGUAGE TypeFamilies, ExistentialQuantification, RankNTypes, ConstraintKinds, FlexibleInstances #-}

module Druid.WXExtensions where

{-import qualified Graphics.UI.WX as WX  hiding ((:=))-}
{-import Graphics.UI.WX(Prop((:=)), Attr(..))-}
import Graphics.UI.WX

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Data.IORef

import Druid.DruidMonad

import Debug.Trace

---------------------------------------------------------------------------------
-- Classes for graphics components and containers
---------------------------------------------------------------------------------

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
-- "Abstract" class for simple graphics
---------------------------------------------------------------------------------

data SimpleGraphicsData = SimpleGraphicsData 
  { sgdContainer :: Maybe AnyGraphicsContainer, sgdBounds :: Rect, sgdBestSize :: Size }

class SimpleGraphics g where
  getSimpleGraphicsData :: g -> IORef SimpleGraphicsData
  draw :: g -> DC () -> IO ()

fetchSimpleGraphicsData :: SimpleGraphics g => g -> (SimpleGraphicsData -> a) -> IO a
fetchSimpleGraphicsData g processFn = processFn <$> readIORef (getSimpleGraphicsData g)

updateSimpleGraphicsData :: SimpleGraphics g => g -> (SimpleGraphicsData -> SimpleGraphicsData) -> IO ()
updateSimpleGraphicsData g processFn = modifyIORef (getSimpleGraphicsData g) processFn


newtype SimpleGraphicsWrapper a = SimpleGraphicsWrapper { unwrapSimpleGraphics :: a }

instance SimpleGraphics g => SimpleGraphics (SimpleGraphicsWrapper g) where
  getSimpleGraphicsData g = getSimpleGraphicsData $ unwrapSimpleGraphics g
  draw g dc = draw (unwrapSimpleGraphics g) dc


instance SimpleGraphics g => GraphicsComponent (SimpleGraphicsWrapper g) where
  onAttachToContainer g w = updateSimpleGraphicsData g (\r -> r { sgdContainer = Just $ AnyGraphicsContainer w} )
  onDetachFromContainer g = updateSimpleGraphicsData g (\r -> r { sgdContainer = Nothing} )
  onDraw g dc = draw g dc


instance SimpleGraphics g => Dimensions (SimpleGraphicsWrapper g) where
  outerSize = newAttr "outerSize" readSize writeSize where
    readSize g = fetchSimpleGraphicsData g (rectSize . sgdBounds)
    writeSize g sz = updateSimpleGraphicsData g (\r -> r { sgdBounds = resizeRect (sgdBounds r) sz })
  position  = newAttr "position" readPos writePos where
    readPos g = fetchSimpleGraphicsData g (rectTopLeft . sgdBounds)
    writePos g pt = updateSimpleGraphicsData g (\r -> r { sgdBounds = repositionRect (sgdBounds r) pt})
  area = newAttr "area" readArea writeArea where
    readArea g = fetchSimpleGraphicsData g sgdBounds
    writeArea g ar = updateSimpleGraphicsData g (\r -> r { sgdBounds = ar})
  bestSize = readAttr "bestSize" (flip fetchSimpleGraphicsData $ sgdBestSize)
  clientSize = newAttr "clientSize" readSize writeSize where
    readSize g = fetchSimpleGraphicsData g (rectSize . sgdBounds)
    writeSize g sz = updateSimpleGraphicsData g (\r -> r { sgdBounds = resizeRect (sgdBounds r) sz })
  virtualSize = newAttr "virtualSize" readSize writeSize where
    readSize g = fetchSimpleGraphicsData g (rectSize . sgdBounds)
    writeSize g sz = updateSimpleGraphicsData g (\r -> r { sgdBounds = resizeRect (sgdBounds r) sz })


----------------------------------------------------------------------------------
-- Basic Graphics components
----------------------------------------------------------------------------------

data Rectangle_ = Rectangle (IORef SimpleGraphicsData)

instance SimpleGraphics Rectangle_ where
  getSimpleGraphicsData (Rectangle sgd) = sgd
  draw g dc = fetchSimpleGraphicsData g sgdBounds >>= (\bounds -> drawRect dc bounds [])

type Rectangle = SimpleGraphicsWrapper Rectangle_  

----------------------------------------------------------------------------------
-- Basic Graphics Containers
----------------------------------------------------------------------------------


----------------------------------------------------------------------------------
-- General WX helpers
----------------------------------------------------------------------------------

resizeRect :: Rect -> Size -> Rect
resizeRect rc sz = rect (rectTopLeft rc) sz

repositionRect :: Rect -> Point -> Rect
repositionRect rc pt = rect pt (rectSize rc)

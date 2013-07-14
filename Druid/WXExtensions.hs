{-# LANGUAGE TypeFamilies, ExistentialQuantification, RankNTypes, ConstraintKinds, FlexibleInstances #-}

module Druid.WXExtensions where

{-import qualified Graphics.UI.WX as WX  hiding ((:=))-}
{-import Graphics.UI.WX(Prop((:=)), Attr(..))-}
import Graphics.UI.WX

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Data.IORef
import Data.List
import Data.Unique
import System.IO.Unsafe

import qualified Data.HashTable.IO as H

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
  getComponentId :: g -> Integer
  onAttachToContainer :: GraphicsContainer w => g -> w -> IO ()
  onDetachFromContainer :: g -> IO ()
  onDraw :: g -> DC () -> IO ()


data AnyGraphicsContainer = forall w. GraphicsContainer w => AnyGraphicsContainer w

data AnyGraphicsComponent = forall w. GraphicsComponent w => AnyGraphicsComponent w

instance Eq AnyGraphicsComponent where
  (AnyGraphicsComponent w1) == (AnyGraphicsComponent w2) = (getComponentId w1) == (getComponentId w2)

instance Ord AnyGraphicsComponent where
  compare (AnyGraphicsComponent w1) (AnyGraphicsComponent w2) = 
    compare (getComponentId w1) (getComponentId w2)

---------------------------------------------------------------------------------
-- "Abstract" class for simple graphics
---------------------------------------------------------------------------------

data SimpleGraphicsData = SimpleGraphicsData 
  { sgdContainer :: Maybe AnyGraphicsContainer, sgdBounds :: Rect, sgdBestSize :: Size }

class SimpleGraphics g where
  getSimpleGraphicsData :: g -> IORef SimpleGraphicsData
  getId :: g -> Integer
  draw :: g -> DC () -> IO ()

fetchSimpleGraphicsData :: SimpleGraphics g => g -> (SimpleGraphicsData -> a) -> IO a
fetchSimpleGraphicsData g processFn = processFn <$> readIORef (getSimpleGraphicsData g)

updateSimpleGraphicsData :: SimpleGraphics g => g -> (SimpleGraphicsData -> SimpleGraphicsData) -> IO ()
updateSimpleGraphicsData g processFn = modifyIORef (getSimpleGraphicsData g) processFn


newtype SimpleGraphicsWrapper a = SimpleGraphicsWrapper { unwrapSimpleGraphics :: a }

instance SimpleGraphics g => SimpleGraphics (SimpleGraphicsWrapper g) where
  getSimpleGraphicsData g = getSimpleGraphicsData $ unwrapSimpleGraphics g
  draw g dc = draw (unwrapSimpleGraphics g) dc
  getId g = getId (unwrapSimpleGraphics g)


instance SimpleGraphics g => GraphicsComponent (SimpleGraphicsWrapper g) where
  onAttachToContainer g w = updateSimpleGraphicsData g (\r -> r { sgdContainer = Just $ AnyGraphicsContainer w} )
  onDetachFromContainer g = updateSimpleGraphicsData g (\r -> r { sgdContainer = Nothing} )
  onDraw g dc = draw g dc
  getComponentId g = getId g


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

getNextComponentId :: IO Integer
getNextComponentId = newUnique >>= \u -> return . fromIntegral $ hashUnique u


data Rectangle_ = Rectangle (Integer, IORef SimpleGraphicsData)

instance SimpleGraphics Rectangle_ where
  getSimpleGraphicsData (Rectangle (_, sgd)) = sgd
  draw g dc = fetchSimpleGraphicsData g sgdBounds >>= (\bounds -> drawRect dc bounds [])
  getId (Rectangle (id, _))= id

type Rectangle = SimpleGraphicsWrapper Rectangle_  

createRectangle :: GraphicsContainer w => w -> [Prop Rectangle] -> IO Rectangle
createRectangle w props = do
  id <- getNextComponentId
  sgdRef <- newIORef $ makeInitialRecord w
  makeRectangle id sgdRef
  where
    makeInitialRecord w = SimpleGraphicsData {
      sgdContainer = Just $ AnyGraphicsContainer w, 
      sgdBounds = rectFromSize $ sz 100 100, 
      sgdBestSize = sz 100 100}
    makeRectangle id sgdRef = do
      let rectangle = SimpleGraphicsWrapper $ Rectangle (id, sgdRef)
      set rectangle props
      return rectangle

----------------------------------------------------------------------------------
-- Basic Graphics Containers
----------------------------------------------------------------------------------

type GraphicsComponentCollection = H.BasicHashTable Int [AnyGraphicsComponent]

graphicsComponentData :: GraphicsComponentCollection
graphicsComponentData = unsafePerformIO $ H.new

addGraphicsComponent :: GraphicsComponentCollection -> Int -> AnyGraphicsComponent -> IO ()
addGraphicsComponent collection id g = do
  maybeList <- H.lookup collection id
  H.insert collection id $ maybe [g] (g :) maybeList

removeGraphicsComponent :: GraphicsComponentCollection -> Int -> AnyGraphicsComponent -> IO ()
removeGraphicsComponent collection id g = do
  maybeList <- H.lookup collection id
  H.insert collection id $ maybe [] (delete g) maybeList



instance GraphicsContainer (Frame a) where
  addGraphics fr g = get fr identity >>= \id -> addGraphicsComponent graphicsComponentData id (AnyGraphicsComponent g)
  removeGraphics fr g = get fr identity >>= \id -> removeGraphicsComponent graphicsComponentData id (AnyGraphicsComponent g)
  requestRepaint fr = repaint fr
  

----------------------------------------------------------------------------------
-- General WX helpers
----------------------------------------------------------------------------------

resizeRect :: Rect -> Size -> Rect
resizeRect rc sz = rect (rectTopLeft rc) sz

repositionRect :: Rect -> Point -> Rect
repositionRect rc pt = rect pt (rectSize rc)

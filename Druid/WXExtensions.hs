{-# LANGUAGE TypeFamilies, ExistentialQuantification, RankNTypes, ConstraintKinds, FlexibleInstances, ScopedTypeVariables #-}

module Druid.WXExtensions where

import Graphics.UI.WX
import Graphics.UI.WXCore (imageCreateFromBitmap, imageConvertToBitmap, imageScale, 
                           bitmapDelete, imageDelete, imageGetSize, windowRefreshRect)
import qualified Graphics.UI.WXCore as WXC

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Data.IORef
import Data.List
import Data.Maybe
import Data.Unique
import System.IO
import System.IO.Unsafe
    
import Debug.Trace

import qualified Data.HashTable.IO as H

---------------------------------------------------------------------------------
-- Classes for graphics components and containers
---------------------------------------------------------------------------------

class GraphicsContainer w where
  addGraphics :: GraphicsComponent g => w -> g -> IO ()
  removeGraphics :: GraphicsComponent g => w -> g -> IO ()
  requestRepaint :: w -> Rect -> IO ()

class GraphicsComponent g where
  getComponentId :: g -> Int
  onAttachToContainer :: GraphicsContainer w => g -> w -> IO ()
  onDetachFromContainer :: g -> IO ()
  onDraw :: g -> DC () -> IO ()


data AnyGraphicsContainer = forall w. GraphicsContainer w => AnyGraphicsContainer w

instance GraphicsContainer AnyGraphicsContainer where
  addGraphics (AnyGraphicsContainer w) = addGraphics w 
  removeGraphics (AnyGraphicsContainer w) = removeGraphics w 
  requestRepaint (AnyGraphicsContainer w) r = requestRepaint w r

data AnyGraphicsComponent = forall w. GraphicsComponent w => AnyGraphicsComponent w

instance Eq AnyGraphicsComponent where
  (AnyGraphicsComponent w1) == (AnyGraphicsComponent w2) = getComponentId w1 == getComponentId w2

instance Ord AnyGraphicsComponent where
  compare (AnyGraphicsComponent w1) (AnyGraphicsComponent w2) = 
    compare (getComponentId w1) (getComponentId w2)

instance GraphicsComponent AnyGraphicsComponent where
  getComponentId (AnyGraphicsComponent g) = getComponentId g
  onAttachToContainer (AnyGraphicsComponent g) = onAttachToContainer g
  onDetachFromContainer (AnyGraphicsComponent g) = onDetachFromContainer g
  onDraw (AnyGraphicsComponent g) = onDraw g


---------------------------------------------------------------------------------
-- "Abstract" class for simple graphics
---------------------------------------------------------------------------------

data SimpleGraphicsData = SimpleGraphicsData 
  { sgdContainer :: Maybe AnyGraphicsContainer, 
    sgdBounds :: Rect, sgdBestSize :: Size, 
    sgdBackground :: Color, sgdForeground :: Color}

class SimpleGraphics g where
  getSimpleGraphicsData :: g -> IO SimpleGraphicsData
  setSimpleGraphicsData :: g -> SimpleGraphicsData -> IO ()
  getId :: g -> Int
  draw :: g -> DC () -> IO ()
  notify :: g -> Rect -> IO ()

  notify g r = fetchSimpleGraphicsData g sgdContainer >>= \c -> maybe (return ()) (flip requestRepaint r) c

fetchSimpleGraphicsData :: SimpleGraphics g => g -> (SimpleGraphicsData -> a) -> IO a
fetchSimpleGraphicsData g processFn = processFn <$> getSimpleGraphicsData g

updateSimpleGraphicsData :: SimpleGraphics g => g -> (SimpleGraphicsData -> SimpleGraphicsData) -> IO ()
updateSimpleGraphicsData g processFn = getSimpleGraphicsData g >>= return . processFn >>= setSimpleGraphicsData g


newtype SimpleGraphicsWrapper a = SimpleGraphicsWrapper { unwrapSimpleGraphics :: a }

instance SimpleGraphics g => SimpleGraphics (SimpleGraphicsWrapper g) where
  getSimpleGraphicsData g = getSimpleGraphicsData $ unwrapSimpleGraphics g
  setSimpleGraphicsData g dta = setSimpleGraphicsData (unwrapSimpleGraphics g) dta
  draw g dc = draw (unwrapSimpleGraphics g) dc
  getId g = getId (unwrapSimpleGraphics g)
  notify g r = notify (unwrapSimpleGraphics g) r


instance SimpleGraphics g => GraphicsComponent (SimpleGraphicsWrapper g) where
  onAttachToContainer g w = updateSimpleGraphicsData g (\r -> r { sgdContainer = Just $ AnyGraphicsContainer w} )
  onDetachFromContainer g = updateSimpleGraphicsData g (\r -> r { sgdContainer = Nothing} )
  onDraw g dc = draw g dc
  getComponentId g = getId g


instance SimpleGraphics g => Dimensions (SimpleGraphicsWrapper g) where
  outerSize = newAttr "outerSize" readSize writeSize where
    readSize g = fetchSimpleGraphicsData g (rectSize . sgdBounds)
    writeSize g s = do 
      oldBounds <- fetchSimpleGraphicsData g sgdBounds
      let newBounds = resizeRect oldBounds s
      updateSimpleGraphicsData g (\r -> r { sgdBounds = newBounds }) 
      notify g (inflateRect (sz 4 4) $ rectUnion oldBounds newBounds)
  position  = newAttr "position" readPos writePos where
    readPos g = fetchSimpleGraphicsData g (rectTopLeft . sgdBounds)
    writePos g pt = do
      oldBounds <- fetchSimpleGraphicsData g sgdBounds
      let newBounds = repositionRect oldBounds pt
      updateSimpleGraphicsData g (\r -> r { sgdBounds = newBounds}) 
      notify g (inflateRect (sz 4 4) $ rectUnion oldBounds newBounds)
  area = newAttr "area" readArea writeArea where
    readArea g = fetchSimpleGraphicsData g sgdBounds
    writeArea g ar = updateSimpleGraphicsData g (\r -> r { sgdBounds = ar}) >> notify g ar
  bestSize = readAttr "bestSize" (flip fetchSimpleGraphicsData sgdBestSize)
  clientSize = newAttr "clientSize" readSize writeSize where
    readSize g = fetchSimpleGraphicsData g (rectSize . sgdBounds)
    writeSize g s = do
      oldBounds <- fetchSimpleGraphicsData g sgdBounds
      let newBounds = resizeRect oldBounds s
      updateSimpleGraphicsData g (\r -> r { sgdBounds = newBounds }) 
      notify g (inflateRect (sz 4 4) $ rectUnion oldBounds newBounds)
  virtualSize = newAttr "virtualSize" readSize writeSize where
    readSize g = fetchSimpleGraphicsData g (rectSize . sgdBounds)
    writeSize g s = do
      oldBounds <- fetchSimpleGraphicsData g sgdBounds
      let newBounds = resizeRect oldBounds s
      updateSimpleGraphicsData g (\r -> r { sgdBounds = newBounds }) 
      notify g (inflateRect (sz 4 4) $ rectUnion oldBounds newBounds)

instance SimpleGraphics g => Colored (SimpleGraphicsWrapper g) where
  bgcolor = newAttr "bgcolor" readBgColor writeBgColor where
    readBgColor g = fetchSimpleGraphicsData g sgdBackground
    writeBgColor g color = do
      bounds <- fetchSimpleGraphicsData g sgdBounds
      updateSimpleGraphicsData g (\r -> r { sgdBackground = color }) 
      notify g bounds
  color = newAttr "color" readFgColor writeFgColor where
    readFgColor g = fetchSimpleGraphicsData g sgdForeground
    writeFgColor g color = do
      bounds <- fetchSimpleGraphicsData g sgdBounds
      updateSimpleGraphicsData g (\r -> r { sgdForeground = color }) 
      notify g bounds

instance SimpleGraphics g => Identity (SimpleGraphicsWrapper g) where
  identity = readAttr "identity" (return . getId)

----------------------------------------------------------------------------------
-- Basic Graphics components
----------------------------------------------------------------------------------

getNextComponentId :: IO Int
getNextComponentId = newUnique >>= \u -> return . fromIntegral $ hashUnique u


data Rectangle_ = Rectangle (Int, IORef SimpleGraphicsData)

instance SimpleGraphics Rectangle_ where
  getId (Rectangle (wid, _))= wid
  getSimpleGraphicsData (Rectangle (_, sgd)) = readIORef sgd
  setSimpleGraphicsData (Rectangle (_, sgd)) dta = writeIORef sgd dta
  draw g dc = do
    bounds <- fetchSimpleGraphicsData g sgdBounds 
    bg <- fetchSimpleGraphicsData g sgdBackground
    fg <- fetchSimpleGraphicsData g sgdForeground
    drawRect dc bounds [bgcolor := bg, color := fg]

type Rectangle = SimpleGraphicsWrapper Rectangle_  

createRectangle :: GraphicsContainer w => w -> [Prop Rectangle] -> IO Rectangle
createRectangle w props = do
  id <- getNextComponentId
  sgdRef <- newIORef makeInitialRecord
  rectangle <- makeRectangle id sgdRef
  addGraphics w rectangle
  return rectangle
  where
    makeInitialRecord = SimpleGraphicsData {
      sgdContainer = Nothing,
      sgdBounds = rectFromSize $ sz 100 100, 
      sgdBestSize = sz 100 100,
      sgdBackground = white,
      sgdForeground = black}
    makeRectangle id sgdRef = do
      let rectangle = SimpleGraphicsWrapper $ Rectangle (id, sgdRef)
      set rectangle props
      return rectangle



data Ellipse_ = Ellipse (Int, IORef SimpleGraphicsData)

instance SimpleGraphics Ellipse_ where
  getId (Ellipse (wid, _))= wid
  getSimpleGraphicsData (Ellipse (_, sgd)) = readIORef sgd
  setSimpleGraphicsData (Ellipse (_, sgd)) dta = writeIORef sgd dta
  draw g dc = do
    bounds <- fetchSimpleGraphicsData g sgdBounds 
    bg <- fetchSimpleGraphicsData g sgdBackground
    fg <- fetchSimpleGraphicsData g sgdForeground
    ellipse dc bounds [bgcolor := bg, color := fg]

type Ellipse = SimpleGraphicsWrapper Ellipse_  

createEllipse :: GraphicsContainer w => w -> [Prop Ellipse] -> IO Ellipse
createEllipse w props = do
  id <- getNextComponentId
  sgdRef <- newIORef makeInitialRecord
  ellipse <- makeEllipse id sgdRef
  addGraphics w ellipse
  return ellipse           
  where
    makeInitialRecord = SimpleGraphicsData {
      sgdContainer = Nothing,
      sgdBounds = rectFromSize $ sz 100 100, 
      sgdBestSize = sz 100 100,
      sgdBackground = white,
      sgdForeground = black}
    makeEllipse id sgdRef = do
      let ellipse = SimpleGraphicsWrapper $ Ellipse (id, sgdRef)
      set ellipse props
      return ellipse


data ImageData = ImageData { imdSgd :: SimpleGraphicsData, imdFilePath :: FilePath, imdImage :: Maybe (WXC.Image ()) }

data Image_ = Image (Int, IORef ImageData)

instance SimpleGraphics Image_ where
  getId (Image (wid, _))= wid
  getSimpleGraphicsData (Image (_, imd)) = readIORef imd >>= return . imdSgd
  setSimpleGraphicsData (Image (_, imd)) dta = modifyIORef imd (\r -> r { imdSgd = dta})
  draw g dc = fetchImg g >>= \img -> if (isJust $ img) then drawImg g dc else return ()
    where 
      drawImg g@(Image(_, imd)) dc = do
        img <- fetchImg g >>= return . fromJust
        bounds <- fetchSimpleGraphicsData g sgdBounds 
        bsz <- imageGetSize img
        let wsz = rectSize bounds
        {-let scale = calcScale wsz bsz-}
        let renderSize = scaledSize wsz bsz
        rescaled <- imageScale img renderSize >>= imageConvertToBitmap
        liftIO $ drawBitmap dc rescaled pointZero False []
        bitmapDelete rescaled
      scaledSize w b@(Size bw bh) = let scale = calcScale w b in Size (round $ fromIntegral bw * scale) (round $ fromIntegral bh * scale)
      calcScale (Size ww wh) (Size bw bh) = min (scalew ww bw) (scaleh wh bh)
      scalew ww bw = fromIntegral ww / fromIntegral bw
      scaleh wh bh = fromIntegral wh / fromIntegral bh
      fetchImg (Image (_, imd)) = readIORef imd >>= return . imdImage

type Image = SimpleGraphicsWrapper Image_  

createImage :: GraphicsContainer w => w -> [Prop Image] -> IO Image
createImage w props = do
  id <- getNextComponentId
  imdRef <- newIORef makeInitialRecord
  image <- makeImage id imdRef
  addGraphics w image
  return image           
  where
    makeInitialRecord = ImageData { imdSgd = makeSgd, imdFilePath = "", imdImage = Nothing }
    makeSgd = SimpleGraphicsData {
      sgdContainer = Nothing,
      sgdBounds = rectFromSize $ sz 100 100, 
      sgdBestSize = sz 100 100,
      sgdBackground = white,
      sgdForeground = black}
    makeImage id sgdRef = do
      let image = SimpleGraphicsWrapper $ Image (id, sgdRef)
      set image props
      return image

instance Pictured Image where
  picture = newAttr "picture" readPicture writePicture where
    readPicture img = readIORef (fetchImageDataRef img) >>= return . imdFilePath
    writePicture img fp = do
      image <- imageCreateFromFile fp
      let ref = fetchImageDataRef img
      imgData <- readIORef ref
      maybe (return ()) imageDelete $ imdImage imgData
      writeIORef ref $ imgData { imdFilePath = fp, imdImage = Just image }
      bounds <- fetchSimpleGraphicsData img sgdBounds
      notify img bounds

fetchImageDataRef :: Image -> IORef ImageData
fetchImageDataRef img = let Image(_, dta) = unwrapSimpleGraphics img in dta

----------------------------------------------------------------------------------
-- Basic Graphics Containers
----------------------------------------------------------------------------------

type GraphicsComponentCollection = H.BasicHashTable Int [AnyGraphicsComponent]

graphicsComponentData :: GraphicsComponentCollection
graphicsComponentData = unsafePerformIO $ H.new

addGraphicsComponent :: GraphicsComponentCollection -> Int -> AnyGraphicsComponent -> IO ()
addGraphicsComponent collection wid g = do
  maybeList <- H.lookup collection wid
  H.insert collection wid $ maybe [g] (g :) maybeList

removeGraphicsComponent :: GraphicsComponentCollection -> Int -> AnyGraphicsComponent -> IO ()
removeGraphicsComponent collection wid g = do
  maybeList <- H.lookup collection wid
  H.insert collection wid $ maybe [] (delete g) maybeList


getGraphicsComponents :: GraphicsComponentCollection -> Int -> IO [AnyGraphicsComponent]
getGraphicsComponents collection wid = do
  maybeList <- H.lookup collection wid
  return $ maybe [] (\x -> x) maybeList

paintComponents :: GraphicsComponentCollection -> Int -> DC () -> IO ()
paintComponents collection wid dc = do
  components <- getGraphicsComponents graphicsComponentData wid
  mapM_ (\(AnyGraphicsComponent g) -> onDraw g dc) components


instance GraphicsContainer (Frame a) where
  addGraphics fr g = get fr identity >>= \wid -> addGraphicsComponent graphicsComponentData wid (AnyGraphicsComponent g) >> onAttachToContainer g fr
  removeGraphics fr g = get fr identity >>= \wid -> removeGraphicsComponent graphicsComponentData wid (AnyGraphicsComponent g) >> onDetachFromContainer g
  requestRepaint fr r = windowRefreshRect fr False r -- repaint fr
  
createFrame :: [Prop (Frame ())] -> IO (Frame ())
createFrame props = do
  fr <- frame props
  set fr [on paint := (painter fr)]
  return fr
  where
    painter fr dc _ = paintBackground fr dc >> get fr identity >>= \wid -> paintComponents graphicsComponentData wid dc
      
createFrameEx :: ([Prop (Frame a)] -> IO (Frame a)) -> [Prop (Frame a)] -> IO (Frame a)
createFrameEx constructor props = do
  fr <- constructor props
  set fr [on paint := (painter fr)]
  return fr
  where
    painter fr dc _ = paintBackground fr dc >> get fr identity >>= \wid -> paintComponents graphicsComponentData wid dc

instance GraphicsContainer (Panel a) where
  addGraphics pl g = get pl identity >>= \id -> addGraphicsComponent graphicsComponentData id (AnyGraphicsComponent g) >> onAttachToContainer g pl
  removeGraphics pl g = get pl identity >>= \id -> removeGraphicsComponent graphicsComponentData id (AnyGraphicsComponent g) >> onDetachFromContainer g
  requestRepaint pl r = windowRefreshRect pl False r -- repaint pl
  
createPanel :: Window a -> [Prop (Panel ())] -> IO (Panel ())
createPanel w props = do
  pl <- panel w props
  set pl [on paint := (painter pl)]
  liftIO . traceIO $ "Creating panel"
  return pl
  where
    painter pl dc rc = paintBackground pl dc >> get pl identity >>= \id -> paintComponents graphicsComponentData id dc
      

----------------------------------------------------------------------------------
-- General WX helpers
----------------------------------------------------------------------------------

resizeRect :: Rect -> Size -> Rect
resizeRect rc s = rect (rectTopLeft rc) s

repositionRect :: Rect -> Point -> Rect
repositionRect rc p = rect p (rectSize rc)

inflateRect :: Size -> Rect -> Rect
inflateRect s rc = rectBetween (pointSub topLeft halfSzAsPt) (pointAdd bottomRight halfSzAsPt)
  where 
    topLeft = rectTopLeft rc
    bottomRight = rectBottomRight rc
    halfSzAsPt = pt (sizeW s `quot` 2) (sizeH s `quot` 2)


paintBackground :: (Dimensions w, Colored w, Paint w) => w -> DC () -> IO ()
paintBackground w dc = do
  bg <- get w bgcolor
  s <- get w clientSize
  drawRect dc (rectFromSize s) [brushColor := bg, penColor := bg]


removeSimpleGraphics :: SimpleGraphics g => SimpleGraphicsWrapper g -> IO ()
removeSimpleGraphics graphics = do
  gparent <- fetchSimpleGraphicsData graphics sgdContainer
  maybe (return ()) (flip removeGraphics graphics) gparent


{-requestParentRepaint :: SimpleGraphics g => SimpleGraphicsWrapper g -> IO ()-}
{-requestParentRepaint graphics = do-}
  {-gparent <- fetchSimpleGraphicsData graphics sgdContainer-}
  {-maybe (return ()) requestRepaint gparent-}

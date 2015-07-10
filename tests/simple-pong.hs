{-# LANGUAGE ViewPatterns, ExistentialQuantification  #-}

module Main where

import Druid.WX.Frp
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Debug.Trace
import System.Random
import Control.Applicative

import Graphics.UI.WX hiding (Event)
import Graphics.UI.WXCore hiding (Event)

-- Simple pong implementation

------------------------- Types -----------------------

type Ball = WXEllipse

type Paddle = WXRectangle

------------------------- Configurations -----------------------

ballSize = sz 20 20

paddleSize = sz 40 10

gameSpace = rectFromSize $ sz 300 300

effectiveGameSpace = rectFromSize $ sz (rectWidth gameSpace) (rectHeight gameSpace - sizeH paddleSize)

------------------------- Main UI -----------------------

gui :: Druid ()
gui = do 
  frame <- createWXFrameFixed [text :=~ "Pong", clientSize :=~ sz 350 300]
  gamePanel <- createWXPanel frame [bgcolor :=~ blue, style :=~ 0]
  (buttonPanel, startButton, stopButton) <- makeButtonPanel frame
  (statusPanel, score) <- makeStatusPanel frame
  let ly = column 10 [row 10 [static $ minsize (sz 300 300) (getLayout gamePanel),
                              static $ getLayout buttonPanel], 
                      hfill (getLayout statusPanel)]
  setAttrs frame [layout :=~ static ly]
  timer <- createWXTimer gamePanel [interval :=~ 20, enabled :=~ False]
  ball <- makeBall gamePanel
  paddle <- makePaddle gamePanel
  (lKey, rKey) <- getLeftAndRightKeyEvents [AnyReactiveEventSource frame, AnyReactiveEventSource statusPanel,
                                            AnyReactiveEventSource gamePanel, AnyReactiveEventSource buttonPanel]
  startCommand <- onCommand startButton
  react frame startCommand $ const2 $ startGame ball paddle timer score lKey rKey
  stopCommand <- onCommand stopButton
  react frame stopCommand $ const2 $ stopGame timer
  timerState <- getAttr timer enabled
  setAttr startButton enabled (lift1 not timerState)
  setAttr stopButton enabled timerState
  hitTest <- paddleHitTest ball paddle
  react frame hitTest $ \_ r -> liftIO $ putStrLn (show r)
  return ()
  where
    makeStatusPanel parent = do
      statusPanel <- createWXPanel parent [bgcolor :=~ red]
      desc <- createWXLabel statusPanel [text :=~ "Use arrow keys to move"]
      score <- createWXLabel statusPanel [text :=~ "Score:  0"]
      let ly = row 5 [hfill (getLayout desc), rigid (getLayout score)]
      setAttrs statusPanel  [layout :=~ ly]
      return (statusPanel, score)
    makeBall parent = createWXEllipse parent [area :=~ rect (pt (-100) (-100)) ballSize]
    makePaddle parent = createWXRectangle parent [area :=~ rect (pt 0 (rectHeight effectiveGameSpace)) paddleSize]
    makeButtonPanel parent = do
      buttonPanel <- createWXPanel parent []
      startButton <- createWXButton buttonPanel [text :=~ "Start"]
      stopButton <- createWXButton buttonPanel [text :=~ "Stop"]
      let ly = column 10 [getLayout startButton, getLayout stopButton]
      setAttrs buttonPanel [layout :=~ ly]
      return (buttonPanel, startButton, stopButton)
    const2 = const . const
    
------------------------- Helpers -----------------------


startGame :: Ball -> Paddle -> WXTimer -> WXLabel -> Event () -> Event () -> Druid ()
startGame ball paddle tmr score leftEv rightEv = do
  tickEv <- onCommand tmr
  makeMovingBall ball tickEv
  makeMovingPaddle paddle leftEv rightEv
  setAttrs tmr [enabled :=~ True]
  setAttr score text =<< scoreUpdateSig
  where
    scoreUpdateSig = scoreValueSig >>= return . lift1 (("Score: " ++) . show)
    scoreValueSig = scoreUpdateEv >>= return . (accum 0)
    scoreUpdateEv = paddleHitTest ball paddle >>= return . (==> scoreFn)
    scoreFn v = if v == PaddleHitSuccess then (+ 1) else (+ 0)

stopGame :: WXTimer -> Druid ()
stopGame tmr = setAttrs tmr [enabled :=~ False]


makeMovingBall :: Ball -> Event a -> Druid ()
makeMovingBall ball tickEv = do
  startPos <- liftIO randomPos
  posSig <- makeBouncingVector startPos direction boundingArea tickEv 
  setAttr ball position $ lift1 translateCenterToLeftTop posSig
  where
    boundingArea = 
      let newLeftTop = pointAdd (rectTopLeft effectiveGameSpace) halfBallAsPoint in
      let newRightBottom = pointSub (rectBottomRight effectiveGameSpace) (halfBallAsPoint) in
      rectBetween newLeftTop newRightBottom
    halfBallAsPoint = point (sizeW ballSize `quot` 2) (sizeH ballSize `quot` 2)
    randomPos = do
      x <- randomRIO (rectLeft boundingArea, rectRight boundingArea)
      return $ pt x (rectTop boundingArea)
    direction = vec 5 5
    translateCenterToLeftTop pos = pointSub pos halfBallAsPoint
  

makeBouncingVector :: Point -> Vector -> Rect -> Event a -> Druid (Behavior Point)
makeBouncingVector initial direction bounds tickEv = do
  base <- observe $ traceB "Base: " $ accum initial (tickEv -=> nextPos)
  let hit = {-traceB "Hit Test: " $ -}lift1 hitTest base
  let hasHit = {-traceB "Hit Test (Bool): " $-} lift1 isJust hit
  let hitEvent = traceE "Hit Event: " $ Druid.WX.Frp.when hasHit `snap` hit
  let bounceEvent = hitEvent ==> (\(Just (pos, dir)) -> makeBouncingVector pos dir bounds tickEv)
  return $ base `untilLater` bounceEvent
  where
    nextPos = truncatePos . (pointMove direction)
    onHitLeft pos = if pointX pos + vecX direction < rectLeft bounds then Just (point (rectLeft bounds) (pointY pos), vecNegX direction) else Nothing
    onHitRight pos = if pointX pos + vecX direction > rectRight bounds then Just (point (rectRight bounds) (pointY pos), vecNegX direction) else Nothing
    onHitTop pos = if pointY pos + vecY direction < rectTop bounds then Just (point (pointX pos) (rectTop bounds), vecNegY direction) else Nothing
    onHitBottom pos = if pointY pos + vecY direction > rectBottom bounds then Just (point (pointX pos) (rectBottom bounds), vecNegY direction) else Nothing
    hitTest pos = onHitLeft pos <|> onHitRight pos <|> onHitTop pos <|> onHitBottom pos
    vecNegX v = vec (negate $ vecX v) (vecY v)
    vecNegY v = vec (vecX v) (negate $ vecY v)
    truncatePos pos = 
      let x = min (rectRight bounds) (max (pointX pos) (rectLeft bounds)) in
      let y = min (rectBottom bounds) (max (pointY pos) (rectTop bounds)) in
      point x y

makeMovingPaddle :: Paddle -> Event a -> Event b -> Druid ()
makeMovingPaddle paddle leftEv rightEv = setAttr paddle position $ makeMovingVector startPos
  where
    makeMovingVector initial = {- traceB "Position: " $ -} accum initial (moveLeftEv .|. moveRightEv)
    moveLeftEv = leftEv -=> moveLeft
    moveRightEv = rightEv -=> moveRight
    moveLeft v = let newX = max (pointX v - jumpDistance) 0 in pt newX (pointY v)
    moveRight v = let rightMost = rectRight effectiveGameSpace - sizeW paddleSize in
                  let newX = min (pointX v + jumpDistance) rightMost in
                  pt newX (pointY v)
    jumpDistance = 10
    startPos = pt (rectWidth effectiveGameSpace `quot` 2 - halfPaddleWidth) (rectBottom effectiveGameSpace)
    halfPaddleWidth = sizeW paddleSize `quot` 2
    
data AnyReactiveEventSource = forall w. ReactiveEventSource w => AnyReactiveEventSource w

getLeftAndRightKeyEvents :: [AnyReactiveEventSource] -> Druid (Event(), Event ())
getLeftAndRightKeyEvents sources = do
  initial <- events $ head sources
  foldM foldF initial (tail sources)
  where
    events (AnyReactiveEventSource w) = do
      rKey <- onKey w KeyRight -- >>= return . traceE "Right Key"
      lKey <- onKey w KeyLeft -- >>= return . traceE "Left Key"
      return (lKey, rKey)
    foldF (lkey, rkey) s = do
      (l, r) <- events s
      return (lkey .|. l, rkey .|. r)


data PaddleHitResult = PaddleHitSuccess | PaddleHitFail
  deriving (Show, Eq)

paddleHitTest :: Ball -> Paddle -> Druid(Event PaddleHitResult)
paddleHitTest ball paddle = do
  ballPos <- getAttr ball position
  paddlePos <- getAttr paddle position
  let readyEv = Druid.WX.Frp.when $ lift1 ballAtBottom ballPos
  return $ readyEv `snap` (hitTest ballPos paddlePos)
  where
    ballAtBottom p = pointY p + sizeH ballSize >= rectBottom effectiveGameSpace
    hitTest ballPos paddlePos = lift2 checkIntersectX ballPos paddlePos
    checkIntersectX ballPos paddlePos = 
      let (ballX1, ballX2) = (pointX ballPos, pointX ballPos + sizeW ballSize) in
      let (paddleX1, paddleX2) = (pointX paddlePos, pointX paddlePos + sizeW paddleSize) in
      if (ballX1 >= paddleX1 && ballX1 <= paddleX2) || (ballX2 >= paddleX1 && ballX2 <= paddleX1)
        then PaddleHitSuccess
        else PaddleHitFail

{-getMouseEvents :: [AnyReactiveEventSource] -> Druid (Event Point)-}
{-getMouseEvents sources = do-}
  {-initial <- mEvent $ head sources-}
  {-foldM foldF initial (tail sources)-}
  {-where-}
    {-mEvent (AnyReactiveEventSource w) = liftA2 (.|.) (onMotion w) (onDrag w)-}
    {-foldF ev w = mEvent w >>= return . (ev .|.)-}
      
{-makeTrackingPaddle paddle ev = setAttr paddle position $ lift1 truncatedPaddlePos $ hold (pt 0 0) ev-}
  {-where-}
    {-truncatedPaddlePos pnt = let x = min (pointX pnt) (rectWidth effectiveGameSpace - sizeW paddleSize) in -}
                             {-pt x (rectBottom effectiveGameSpace)-}


main :: IO ()
main = start $ startEngine gui


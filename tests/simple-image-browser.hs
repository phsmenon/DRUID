{-# LANGUAGE ViewPatterns, ScopedTypeVariables  #-}

module Main where

import Druid.WX.Frp

import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad.IO.Class
import Debug.Trace
import System.Directory

import Graphics.UI.WX hiding (Event)


-- Simple Image Browser - Uses the WXImage (one of our graphics controls). Mixes behaviors and reactors

gui :: Druid ()
gui = do 
  f <- createWXFrame [text :=~ "Browser"] --, clientSize :=~ sz 800 600]
  l <- createWXLabel f [text :=~ "Enter directory to browse"]
  dirtxt <- createWXTextEntry f []
  ipanel <- createWXPanel f [bgcolor :=~ green]
  applyButton <- createWXButton f [text :=~ "Apply"]
  frwButton <- createWXButton f [text :=~ ">"]
  bckButton <- createWXButton f [text :=~ "<"]
  r <- createWXImage ipanel [bgcolor :=~ red, position :=~ point 0 0]
  -- Add the controls to a reasonable layout
  let ly = column 10 [hstretch (getLayout l), 
                      hstretch $ row 4 [
                        hfill (getLayout dirtxt), 
                        rigid (getLayout applyButton), 
                        rigid (getLayout frwButton),
                        rigid (getLayout bckButton)], 
                      stretch $ container (getDelegate ipanel) (stretch $ space 100 100)]
  setAttrs f [layout :=~ margin 20 ly]
  -- Unfortunately graphics controls dont participate in layout as yet. So we do the resize for that ourselves
  handleResize ipanel r
  -- When apply button is clicked set up the new directory.
  dirApplyEv <- snap <$> onCommand applyButton <*> getAttr dirtxt text
  react r dirApplyEv $ changeDirectory frwButton bckButton
  where 
  handleResize p g = do
    e <- snap <$> onResize p <*> getAttr p outerSize
    react g e (\gr v -> setAttr gr outerSize $ lift0 v)
    

-- A reactor that handles directory change. It loads a list of files and sets up the
-- forward and backward button behaviors
changeDirectory frwButton bckButton r dir = do
  files <- readDirectory dir
  if isNothing files || null (fromJust files) 
    then liftIO . putStrLn $ dir ++ " is not valid or does not contain loadable pictures"
    else updateBehavior $ fromJust files
  where
    updateBehavior lst = do
      indexBeh <- moveForwardAndBack <$> onCommand frwButton <*> onCommand bckButton <*> pure (length lst)
      let picBeh = lift1 (lst !!) indexBeh
      setAttr r picture picBeh
      
-- Read the list of image file names from a directory
readDirectory :: String -> Druid (Maybe [String])
readDirectory dir = liftIO $ doesDirectoryExist dir >>= \r -> if r then getImageFiles else return Nothing
  where
  getImageFiles = do
    oldDir <- getCurrentDirectory
    setCurrentDirectory dir
    files <- getDirectoryContents dir >>= mapM canonicalizePath
    let extensions = [".png", ".jpg"]
    let supported_files = concatMap (filesWithExtension files) extensions
    setCurrentDirectory oldDir
    return $ Just supported_files
  filesWithExtension files ext = filter (ext `isSuffixOf`) files
  

-- Given 2 events and a maximum index, produce a behavior that moves forwards and backwards
-- in the range (0, index-1)
moveForwardAndBack :: Event a -> Event b -> Int -> Behavior Int
moveForwardAndBack frwEv bckEv maxIndex =
  let cmbEv = (frwEv -=> moveForward) .|. (bckEv -=> moveBackward) in
  accum 0 cmbEv 
  where
    moveForward index = if index < maxIndex - 1 then index + 1 else index
    moveBackward index = if index <= 0 then index else index - 1

main :: IO ()
main = start $ startEngine gui


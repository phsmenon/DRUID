{-# LANGUAGE TypeFamilies, ExistentialQuantification #-}

module Druid.Layout where

import Graphics.UI.WX

class LayoutCalculation l where
  type LayoutProp l :: *
  type LayoutSpec l :: *
  calculateLayout :: LayoutProp l -> [(Rect, LayoutSpec l)] -> [Rect]

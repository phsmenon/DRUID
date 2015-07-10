module Druid.WX.Types where

import Graphics.UI.WX as WX

---------------------- Definitions -------------------------

data WXEvent = 
  WXTextChange String
  | WXCommand String
  | WXSelect String
  | WXMouse String WX.EventMouse
  | WXKeyboard String WX.EventKey
  | WXFocus String Bool
  | WXActivate String Bool
  | WXClosing String
  | WXResize String
  | WXEnter String WX.Point
  | WXLeave String WX.Point
  | WXMotion String WX.Point
  | WXDrag String WX.Point
  | WXClick String WX.Point
  | WXUnclick String WX.Point
  | WXDoubleClick String WX.Point
  | WXClickRight String WX.Point
  | WXUnclickRight String WX.Point
  | WXKey String WX.Key
  deriving (Eq, Show)


--------------------------------------------------------------------------------

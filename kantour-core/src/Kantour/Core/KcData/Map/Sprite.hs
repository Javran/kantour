{-
  Reference: https://gamedev.stackexchange.com/a/116000
 -}
{-# LANGUAGE DeriveGeneric #-}

module Kantour.Core.KcData.Map.Sprite where

import Data.Aeson
import GHC.Generics
import Kantour.Core.KcData.Map.Xywh

data Sprite = Sprite
  { frame :: Xywh Int
  , rotated :: Bool
  , trimmed :: Bool
  , spriteSourceSize :: Xywh Int
  , sourceSize :: Wh Int
  }
  deriving (Generic, Show)

instance FromJSON Sprite

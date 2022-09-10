{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.MapArea (
  MapArea (..),
) where

import qualified Data.Text as T
import qualified Kantour.Core.KcData.Master.Direct.Maparea as D
import Kantour.Core.KcData.Master.Org.Common

data MapArea = MapArea
  { kcType :: Int
  , kcId :: Int
  , name :: T.Text
  }
  deriving (Generic, Show)

instance NFData MapArea

instance FromDirect MapArea where
  type Source MapArea = D.Maparea

  fromDirect D.Maparea {kcType, kcId, name} =
    pure MapArea {kcType, kcId, name}

{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.FurnitureGraph (
  FurnitureGraph (..),
) where

import qualified Data.Text as T
import qualified Kantour.Core.KcData.Master.Direct.Furnituregraph as D
import Kantour.Core.KcData.Master.Org.Common

data FurnitureGraph = FurnitureGraph
  { version :: T.Text
  , fileName :: T.Text
  , kcType :: Int
  , kcId :: Int
  , no :: Int
  }
  deriving (Generic, Show)

instance NFData FurnitureGraph

instance FromDirect FurnitureGraph where
  type Source FurnitureGraph = D.Furnituregraph

  fromDirect D.Furnituregraph {version, filename = fileName, kcType, kcId, no} =
    pure FurnitureGraph {version, fileName, kcType, kcId, no}

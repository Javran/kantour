module Kantour.Core.KcData.Master.Direct.Mapbgm (
  Mapbgm (..),
) where

import Data.Aeson as Aeson
import Kantour.Core.KcData.Master.Direct.Common

data Mapbgm = Mapbgm
  { movingBgm :: Int
  , mapBgm :: [Int]
  , mId :: Int
  , bossBgm :: [Int]
  , mapareaId :: Int
  , no :: Int
  }
  deriving stock (Generic, Show)

instance FromJSON Mapbgm where
  parseJSON = parseKcMstJson [("mId", "id")]

instance NFData Mapbgm
instance HasKnownFields Mapbgm where
  knownFields _ = kcFields "moving_bgm map_bgm id boss_bgm maparea_id no"

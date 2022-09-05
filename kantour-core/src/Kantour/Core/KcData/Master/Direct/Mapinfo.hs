{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.Mapinfo (
  Mapinfo (..),
) where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Kantour.Core.KcData.Master.Direct.Common

data Mapinfo = Mapinfo
  { infotext :: T.Text
  , sallyFlag :: [Int]
  , requiredDefeatCount :: Maybe Int
  , kcId :: Int
  , item :: [Int]
  , maxMaphp :: Maybe Int
  , opetext :: T.Text
  , name :: T.Text
  , mapareaId :: Int
  , no :: Int
  , level :: Int
  }
  deriving stock (Generic, Show)

instance FromJSON Mapinfo where
  parseJSON = parseKcMstJson

instance NFData Mapinfo
instance HasKnownFields Mapinfo where
  knownFields _ =
    kcFields
      "infotext sally_flag required_defeat_count id item max_maphp \
      \opetext name maparea_id no level"

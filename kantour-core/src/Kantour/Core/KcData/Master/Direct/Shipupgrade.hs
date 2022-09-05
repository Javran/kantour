{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.Shipupgrade (
  Shipupgrade (..),
) where

import Data.Aeson as Aeson

import Kantour.Core.KcData.Master.Direct.Common

data Shipupgrade = Shipupgrade
  { kcId :: Int
  , aviationMatCount :: Int
  , upgradeType :: Int
  , drawingCount :: Int
  , upgradeLevel :: Int
  , currentShipId :: Int
  , originalShipId :: Int
  , armsMatCount :: Int
  , sortno :: Int
  , reportCount :: Int
  , catapultCount :: Int
  , boilerCount :: Maybe Int
  }
  deriving stock (Generic, Show)

instance FromJSON Shipupgrade where
  parseJSON = parseKcMstJson

instance NFData Shipupgrade

instance HasKnownFields Shipupgrade where
  knownFields _ =
    kcFields
      "id aviation_mat_count upgrade_type drawing_count \
      \upgrade_level current_ship_id original_ship_id arms_mat_count \
      \sortno report_count catapult_count boiler_count"

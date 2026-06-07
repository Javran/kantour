{-# LANGUAGE DuplicateRecordFields, NoFieldSelectors #-}

module Kantour.Core.KcData.Master.Direct.Shipupgrade (
  Shipupgrade (..),
) where

import Data.Aeson as Aeson

import Kantour.Core.KcData.Master.Direct.Common

data Shipupgrade = Shipupgrade
  { kcId :: Int
  , upgradeLevel :: Int
  , upgradeType :: Int
  , currentShipId :: Int
  , originalShipId :: Int
  , sortno :: Int
  , aviationMatCount :: Int
  , drawingCount :: Int
  , armsMatCount :: Int
  , reportCount :: Int
  , catapultCount :: Int
  , boilerCount :: Maybe Int
  , techCount :: Int
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
      \sortno report_count catapult_count boiler_count tech_count"

instance Verifiable Shipupgrade where
  verify
    x@( Shipupgrade
          { currentShipId
          , kcId
          , upgradeType
          }
        ) = do
      let warn msg =
            vLogS $
              "Shipupgrade{" <> show currentShipId <> "->" <> show kcId <> "}: " <> msg
      when (upgradeType /= 1) do
        warn "upgradeType is not 1"
      when (currentShipId == 0) $
        case x of
          Shipupgrade
            { aviationMatCount = 0
            , drawingCount = 0
            , upgradeLevel = 0
            , armsMatCount = 0
            , reportCount = 0
            , catapultCount = 0
            , boilerCount = Nothing
            , techCount = 0
            } -> pure ()
          _ -> warn $ "ignored object has non-zero consumption: " <> show x

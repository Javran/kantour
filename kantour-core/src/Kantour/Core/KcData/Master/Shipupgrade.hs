{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Shipupgrade
  ( Shipupgrade (..)
  )
where

import Data.Aeson as Aeson
import Deriving.Aeson
import Kantour.Core.KcData.Master.Common

data Shipupgrade = Shipupgrade
  { shipId :: Int
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
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier (Rename "shipId" "id" : KcConvention)]
          Shipupgrade

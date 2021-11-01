{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Kantour.Core.KcData.Master.Root
  ( MasterRoot (..)
  )
where

import Data.Aeson as Aeson
import Deriving.Aeson
import Kantour.Core.KcData.Master.Common
import Kantour.Core.KcData.Master.Ship

{-
  Root object of master data.
 -}
data MasterRoot = MasterRoot
  { mstSlotitem :: [Value]
  , mstShipgraph :: [Value]
  , mstShip :: [Ship]
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier KcConvention]
          MasterRoot

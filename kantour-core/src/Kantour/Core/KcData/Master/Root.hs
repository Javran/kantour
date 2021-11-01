{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Kantour.Core.KcData.Master.Root
  ( MasterRoot (..)
  )
where

import Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Common
import Kantour.Core.KcData.Master.Ship
import Kantour.Core.KcData.Master.Shipgraph
import Kantour.Core.KcData.Master.Slotitem

{-
  Root object of master data.
 -}
data MasterRoot = MasterRoot
  { mstSlotitem :: [Slotitem]
  , mstShipgraph :: [Shipgraph]
  , mstShip :: [Ship]
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier KcConvention]
          MasterRoot

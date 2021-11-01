{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Kantour.Core.KcData.Master.Root
  ( MasterRoot (..)
  )
where

import Data.Aeson as Aeson
import Deriving.Aeson
import Kantour.Core.KcData.Master.Common
import Kantour.Core.KcData.Master.Ship
import Kantour.Core.KcData.Master.Shipgraph
import Kantour.Core.KcData.Master.Slotitem
import Kantour.Core.KcData.Master.Bgm
import Kantour.Core.KcData.Master.ItemShop

{-
  Root object of master data.
 -}
data MasterRoot = MasterRoot
  { mstSlotitem :: [Slotitem]
  , mstShipgraph :: [Shipgraph]
  , mstShip :: [Ship]
  , mstEquipExslot :: [Int]
  , mstBgm :: [Bgm]
  , mstItemShop :: ItemShop
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier KcConvention]
          MasterRoot

instance HasKnownFields MasterRoot where
  knownFields _ =
    kcFields
      "mst_equip_exslot \
      \mst_bgm mst_item_shop"

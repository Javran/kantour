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
import Kantour.Core.KcData.Master.Bgm
import Kantour.Core.KcData.Master.Common
import Kantour.Core.KcData.Master.Const
import Kantour.Core.KcData.Master.EquipExslotShip
import Kantour.Core.KcData.Master.EquipShip
import Kantour.Core.KcData.Master.Furniture
import Kantour.Core.KcData.Master.Furnituregraph
import Kantour.Core.KcData.Master.ItemShop
import Kantour.Core.KcData.Master.Maparea
import Kantour.Core.KcData.Master.Mapbgm
import Kantour.Core.KcData.Master.Mapinfo
import Kantour.Core.KcData.Master.Mission
import Kantour.Core.KcData.Master.Payitem
import Kantour.Core.KcData.Master.Ship
import Kantour.Core.KcData.Master.Shipgraph
import Kantour.Core.KcData.Master.Shipupgrade
import Kantour.Core.KcData.Master.Slotitem
import Kantour.Core.KcData.Master.SlotitemEquiptype
import Kantour.Core.KcData.Master.Stype
import Kantour.Core.KcData.Master.Useitem

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
  , mstConst :: Const
  , mstEquipExslotShip :: [EquipExslotShip]
  , mstEquipShip :: [EquipShip]
  , mstFurniture :: [Furniture]
  , mstFurnituregraph :: [Furnituregraph]
  , mstMaparea :: [Maparea]
  , mstMapbgm :: [Mapbgm]
  , mstMapinfo :: [Mapinfo]
  , mstMission :: [Mission]
  , mstPayitem :: [Payitem]
  , mstShipupgrade :: [Shipupgrade]
  , mstSlotitemEquiptype :: [SlotitemEquiptype]
  , mstStype :: [Stype]
  , mstUseitem :: [Useitem]
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
      "mst_const mst_equip_exslot_ship mst_equip_ship mst_equip_exslot \
      \mst_furniture mst_furnituregraph mst_maparea mst_mapbgm mst_mapinfo \
      \mst_bgm mst_item_shop mst_mission mst_payitem mst_ship mst_shipgraph \
      \mst_shipupgrade mst_slotitem mst_slotitem_equiptype mst_stype mst_useitem"

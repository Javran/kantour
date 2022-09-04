module Kantour.Core.KcData.Master.Direct.Root (
  MasterRoot (..),
) where

import Data.Aeson as Aeson
import Deriving.Aeson
import Kantour.Core.KcData.Master.Direct.Bgm
import Kantour.Core.KcData.Master.Direct.Common
import Kantour.Core.KcData.Master.Direct.Const
import Kantour.Core.KcData.Master.Direct.EquipExslotShip
import Kantour.Core.KcData.Master.Direct.EquipShip
import Kantour.Core.KcData.Master.Direct.Furniture
import Kantour.Core.KcData.Master.Direct.Furnituregraph
import Kantour.Core.KcData.Master.Direct.ItemShop
import Kantour.Core.KcData.Master.Direct.Maparea
import Kantour.Core.KcData.Master.Direct.Mapbgm
import Kantour.Core.KcData.Master.Direct.Mapinfo
import Kantour.Core.KcData.Master.Direct.Mission
import Kantour.Core.KcData.Master.Direct.Payitem
import Kantour.Core.KcData.Master.Direct.Ship
import Kantour.Core.KcData.Master.Direct.Shipgraph
import Kantour.Core.KcData.Master.Direct.Shipupgrade
import Kantour.Core.KcData.Master.Direct.Slotitem
import Kantour.Core.KcData.Master.Direct.SlotitemEquiptype
import Kantour.Core.KcData.Master.Direct.Stype
import Kantour.Core.KcData.Master.Direct.Useitem

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

{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.Root (
  Root (..),
) where

import Control.DeepSeq (NFData)
import Data.Aeson as Aeson
import qualified Data.IntSet as IS
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
data Root = Root
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
          Root

instance NFData Root

instance HasKnownFields Root where
  knownFields _ =
    kcFields
      "mst_const mst_equip_exslot_ship mst_equip_ship mst_equip_exslot \
      \mst_furniture mst_furnituregraph mst_maparea mst_mapbgm mst_mapinfo \
      \mst_bgm mst_item_shop mst_mission mst_payitem mst_ship mst_shipgraph \
      \mst_shipupgrade mst_slotitem mst_slotitem_equiptype mst_stype mst_useitem"

instance Verifiable Root where
  verify
    Root
      { mstSlotitem
      , mstShipgraph
      , mstShip
      , mstEquipExslot
      , mstBgm
      , mstItemShop
      , mstConst
      , mstEquipExslotShip
      , mstEquipShip
      , mstFurniture
      , mstFurnituregraph
      , mstMaparea
      , mstMapbgm
      , mstMapinfo
      , mstMission
      , mstPayitem
      , mstShipupgrade
      , mstSlotitemEquiptype
      , mstStype
      , mstUseitem
      } = do
      let verifyListWithUniqueId tag f xs = do
            mapM_ verify xs
            do
              let count = length xs
                  nubCount = IS.size (IS.fromList (fmap f xs))
              when (count /= nubCount) do
                vLogS $ tag <> ": items not unique"

      verifyListWithUniqueId
        "mstSlotitem"
        (\Slotitem {slotId = i} -> i)
        mstSlotitem
      verifyListWithUniqueId
        "mstShipgraph"
        (\Shipgraph {shipId = i} -> i)
        mstShipgraph

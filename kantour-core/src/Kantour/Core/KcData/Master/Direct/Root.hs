{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.Root (
  Root (..),
) where

import Data.List
import Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
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

instance FromJSON Root where
  parseJSON = parseKcMstJson

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
      , mstItemShop = _
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
      let verifyListWithUniqueId tag getId xs = do
            mapM_ verify xs
            forM_ (NE.groupAllWith getId xs) \case
              _ NE.:| [] -> pure ()
              x NE.:| (x1 : _) -> do
                vLogS $ tag <> ": id " <> show (getId x) <> " is not unique"
                vLogS $ tag <> ": " <> show x
                vLogS $ tag <> ": " <> show x1

      verifyListWithUniqueId
        "mstSlotitem"
        (\Slotitem {kcId = i} -> i)
        mstSlotitem

      verifyListWithUniqueId
        "mstShipgraph"
        (\Shipgraph {kcId = i} -> i)
        mstShipgraph

      verifyListWithUniqueId
        "mstShip"
        (\Ship {kcId = i} -> i)
        mstShip

      case findDuplicates mstEquipExslot of
        [] -> pure ()
        xs@(_ : _) ->
          vLogS $ "mstEquipExslot: items are not unique" <> show (fmap (NE.take 2) xs)

      verifyListWithUniqueId
        "mstBgm"
        (\Bgm {kcId = i} -> i)
        mstBgm

      verify mstConst

      verifyListWithUniqueId
        "mstEquipExslotShip"
        (\EquipExslotShip {slotitemId = i} -> i)
        mstEquipExslotShip

      verifyListWithUniqueId
        "mstEquipShip"
        (\EquipShip {shipId = i} -> i)
        mstEquipShip

      verifyListWithUniqueId
        "Furniture"
        (\Furniture {kcId = i} -> i)
        mstFurniture

      verifyListWithUniqueId
        "Furnituregraph"
        (\Furnituregraph {kcId = i} -> i)
        mstFurnituregraph

      verifyListWithUniqueId
        "Maparea"
        (\Maparea {kcId = i} -> i)
        mstMaparea

      verifyListWithUniqueId
        "Mapbgm"
        (\Mapbgm {kcId = i} -> i)
        mstMapbgm

      verifyListWithUniqueId
        "Mapinfo"
        (\Mapinfo {kcId = i} -> i)
        mstMapinfo

      verifyListWithUniqueId
        "Mission"
        (\Mission {kcId = i} -> i)
        mstMission

      verifyListWithUniqueId
        "Payitem"
        (\Payitem {kcId = i} -> i)
        mstPayitem

      {-
        Reference (from main.js):

        - ShipUpgradeModelHolder > ShipUpgradeModelHolderEdit > setData
          (this sets `_dic`, from those items,
          which ignores mst_id_before that are <= 0)

        - ShipUpgradeModel > mst_id_before property,
          which is a direct read of api_current_ship_id.

       -}
      do
        let getId (Shipupgrade {currentShipId = i}) = i
            (kept, ignored) = partition ((> 0) . getId) mstShipupgrade
        mapM_ verify ignored
        verifyListWithUniqueId
          "Shipupgrade"
          getId
          kept

      verifyListWithUniqueId
        "SlotitemEquiptype"
        (\SlotitemEquiptype {kcId = i} -> i)
        mstSlotitemEquiptype

      verifyListWithUniqueId
        "Stype"
        (\Stype {kcId = i} -> i)
        mstStype

      verifyListWithUniqueId
        "Useitem"
        (\Useitem {kcId = i} -> i)
        mstUseitem

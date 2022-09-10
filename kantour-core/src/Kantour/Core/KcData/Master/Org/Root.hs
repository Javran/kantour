{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.Root (
  FromDirect (..),
  Root (..),
) where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import GHC.Generics
import qualified Kantour.Core.KcData.Master.Direct as Direct
import Kantour.Core.KcData.Master.Org.Bgm
import Kantour.Core.KcData.Master.Org.Common
import Kantour.Core.KcData.Master.Org.Const
import Kantour.Core.KcData.Master.Org.Equip
import Kantour.Core.KcData.Master.Org.EquipExslotShip
import Kantour.Core.KcData.Master.Org.EquipShip
import Kantour.Core.KcData.Master.Org.Expedition
import Kantour.Core.KcData.Master.Org.Furniture
import Kantour.Core.KcData.Master.Org.FurnitureGraph
import Kantour.Core.KcData.Master.Org.ItemShop
import Kantour.Core.KcData.Master.Org.MapArea
import Kantour.Core.KcData.Master.Org.MapBgm
import Kantour.Core.KcData.Master.Org.MapInfo
import Kantour.Core.KcData.Master.Org.PayItem
import Kantour.Core.KcData.Master.Org.Ship
import Kantour.Core.KcData.Master.Org.EquipCategory
import Kantour.Core.KcData.Master.Org.ShipGraph
import Kantour.Core.KcData.Master.Org.ShipUpgrade

{-
  Org modules are organized version of the master data.

  I'm still not sure about the API design of Org moduldes right now:

  (1) uniform data type vs. keep different sub-type separated.

    Decision: uniform data type, but keep sub-types separated into smaller Maps.

    - Due to the limitations of GADTs, we can't have common fields that ends up
      in different types, so the plan to tag data with a phantom type won't
      be very convenient to use.

  (2) should we keep unknown / constant fields?

    Currently lean towards dropping them to have a clearer API.
    Worst case we need to have Direct representations around,
    so effectively keeping two copies of master data in memory,
    which I don't think is too bad (less than two copies, actually,
    as I imagine there'll still be some sharing under the hood).

  TODO: WIP on those `:: Direct.*` fields

 -}

data Root = Root
  { equips :: IM.IntMap Equip
  , shipGraphs :: IM.IntMap ShipGraph
  , ships :: IM.IntMap Ship
  , equipExslots :: IS.IntSet
  , bgms :: IM.IntMap Bgm
  , itemShop :: ItemShop
  , kcConst :: Const
  , equipExslotShips :: IM.IntMap EquipExslotShip
  , equipShips :: IM.IntMap EquipShip
  , furnitures :: IM.IntMap Furniture
  , furnitureGraphs :: IM.IntMap FurnitureGraph
  , mapAreas :: IM.IntMap MapArea
  , mapBgms :: IM.IntMap MapBgm
  , mapInfos :: IM.IntMap MapInfo
  , expeditions :: IM.IntMap Expedition
  , payItems :: IM.IntMap PayItem
  , shipUpgrades :: IM.IntMap ShipUpgrade
  , equipCategories :: IM.IntMap EquipCategory
  , stype :: [Direct.Stype]
  , useitem :: [Direct.Useitem]
  }
  deriving (Generic, Show)

instance NFData Root

instance FromDirect Root where
  type Source Root = Direct.Root

  fromDirect
    Direct.Root
      { mstSlotitem
      , mstShipgraph
      , mstShip
      , mstEquipExslot
      , mstBgm
      , mstItemShop = itemShop
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
      , mstStype = stype
      , mstUseitem = useitem
      } = do
      let buildFromList getId xs =
            IM.fromList . fmap (\x -> (getId x, x)) <$> mapM fromDirect xs
      equips <-
        buildFromList (\Equip {kcId = i} -> i) mstSlotitem
      shipGraphs <-
        buildFromList (\ShipGraph {kcId = i} -> i) mstShipgraph
      bgms <-
        buildFromList (\Bgm {kcId = i} -> i) mstBgm
      ships <-
        buildFromList (\Ship {kcId = i} -> i) mstShip
      kcConst <- fromDirect mstConst
      equipExslotShips <-
        buildFromList (\EquipExslotShip {equipId = i} -> i) mstEquipExslotShip
      equipShips <-
        buildFromList (\EquipShip {shipId = i} -> i) mstEquipShip
      furnitures <-
        buildFromList (\Furniture {kcId = i} -> i) mstFurniture
      furnitureGraphs <-
        buildFromList (\FurnitureGraph {kcId = i} -> i) mstFurnituregraph
      mapAreas <-
        buildFromList (\MapArea {kcId = i} -> i) mstMaparea
      mapBgms <-
        buildFromList (\MapBgm {kcId = i} -> i) mstMapbgm
      mapInfos <-
        buildFromList (\MapInfo {kcId = i} -> i) mstMapinfo
      expeditions <-
        buildFromList (\Expedition {kcId = i} -> i) mstMission
      payItems <-
        buildFromList (\PayItem {kcId = i} -> i) mstPayitem
      shipUpgrades <-
        buildFromList (\ShipUpgrade {shipIdFromTo = (i, _)} -> i) $
          filter
            Kantour.Core.KcData.Master.Org.ShipUpgrade.canConvert
            mstShipupgrade
      equipCategories <-
        buildFromList (\EquipCategory {kcId = i} -> i) mstSlotitemEquiptype
      pure
        Root
          { equips
          , shipGraphs
          , ships
          , equipExslots = IS.fromList mstEquipExslot
          , bgms
          , itemShop
          , kcConst
          , equipExslotShips
          , equipShips
          , furnitures
          , furnitureGraphs
          , mapAreas
          , mapBgms
          , mapInfos
          , expeditions
          , payItems
          , shipUpgrades
          , equipCategories
          , stype
          , useitem
          }

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
import Kantour.Core.KcData.Master.Org.Equip
import Kantour.Core.KcData.Master.Org.Ship
import Kantour.Core.KcData.Master.Org.ShipGraph

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
  , itemShop :: Direct.ItemShop
  , const :: Direct.Const
  , equipExslotShip :: [Direct.EquipExslotShip]
  , equipShip :: [Direct.EquipShip]
  , furniture :: [Direct.Furniture]
  , furnituregraph :: [Direct.Furnituregraph]
  , maparea :: [Direct.Maparea]
  , mapbgm :: [Direct.Mapbgm]
  , mapinfo :: [Direct.Mapinfo]
  , mission :: [Direct.Mission]
  , payitem :: [Direct.Payitem]
  , shipupgrade :: [Direct.Shipupgrade]
  , slotitemEquiptype :: [Direct.SlotitemEquiptype]
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
      , mstConst = konst
      , mstEquipExslotShip = equipExslotShip
      , mstEquipShip = equipShip
      , mstFurniture = furniture
      , mstFurnituregraph = furnituregraph
      , mstMaparea = maparea
      , mstMapbgm = mapbgm
      , mstMapinfo = mapinfo
      , mstMission = mission
      , mstPayitem = payitem
      , mstShipupgrade = shipupgrade
      , mstSlotitemEquiptype = slotitemEquiptype
      , mstStype = stype
      , mstUseitem = useitem
      } = do
      let buildFromList getId xs =
            IM.fromList . fmap (\x -> (getId x, x)) <$> mapM fromDirect xs
      equips <- buildFromList (\Equip {kcId = i} -> i) mstSlotitem
      shipGraphs <- buildFromList (\ShipGraph {kcId = i} -> i) mstShipgraph
      bgms <- buildFromList (\Bgm {kcId = i} -> i) mstBgm
      ships <- buildFromList (\Ship {kcId = i} -> i) mstShip
      pure
        Root
          { equips
          , shipGraphs
          , ships
          , equipExslots = IS.fromList mstEquipExslot
          , bgms
          , itemShop
          , const = konst
          , equipExslotShip
          , equipShip
          , furniture
          , furnituregraph
          , maparea
          , mapbgm
          , mapinfo
          , mission
          , payitem
          , shipupgrade
          , slotitemEquiptype
          , stype
          , useitem
          }

{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.Root (
  FromDirect (..),
  Equip (..),
  Root (..),
) where

import qualified Data.IntMap as IM
import GHC.Generics
import qualified Kantour.Core.KcData.Master.Direct as Direct
import Kantour.Core.KcData.Master.Org.Common
import Kantour.Core.KcData.Master.Org.Equip

{-
  Org modules are organized version of the master data.

  TODO: WIP on those `:: Direct.*` fields

 -}

data Root = Root
  { equips :: IM.IntMap Equip
  , -- from slotitem
    shipgraph :: [Direct.Shipgraph]
  , ship :: [Direct.Ship]
  , equipExslot :: [Int]
  , bgm :: [Direct.Bgm]
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
      , mstShipgraph = shipgraph
      , mstShip = ship
      , mstEquipExslot = equipExslot
      , mstBgm = bgm
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
      equips <- buildFromList (\Equip {mstId = i} -> i) mstSlotitem
      pure
        Root
          { equips
          , shipgraph
          , ship
          , equipExslot
          , bgm
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

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

  TODO: WIP on those `:: ()` fields

 -}

data Root = Root
  { equips :: IM.IntMap Equip
  , -- from slotitem
    shipgraph :: ()
  , ship :: ()
  , equipExslot :: ()
  , bgm :: ()
  , itemShop :: ()
  , const :: ()
  , equipExslotShip :: ()
  , equipShip :: ()
  , furniture :: ()
  , furnituregraph :: ()
  , maparea :: ()
  , mapbgm :: ()
  , mapinfo :: ()
  , mission :: ()
  , payitem :: ()
  , shipupgrade :: ()
  , slotitemEquiptype :: ()
  , stype :: ()
  , useitem :: ()
  }
  deriving (Generic, Show)

instance NFData Root

instance FromDirect Root where
  type Source Root = Direct.Root

  fromDirect Direct.Root {mstSlotitem} = do
    es <- mapM (fromDirect @Equip) mstSlotitem
    pure
      Root
        { equips = IM.fromList $ fmap (\e@Equip {eId} -> (eId, e)) es
        , shipgraph = ()
        , ship = ()
        , equipExslot = ()
        , bgm = ()
        , itemShop = ()
        , const = ()
        , equipExslotShip = ()
        , equipShip = ()
        , furniture = ()
        , furnituregraph = ()
        , maparea = ()
        , mapbgm = ()
        , mapinfo = ()
        , mission = ()
        , payitem = ()
        , shipupgrade = ()
        , slotitemEquiptype = ()
        , stype = ()
        , useitem = ()
        }

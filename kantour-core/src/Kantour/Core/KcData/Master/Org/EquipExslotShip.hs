{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.EquipExslotShip (
  EquipExslotShip (..),
) where

import qualified Data.IntSet as IS
import qualified Kantour.Core.KcData.Master.Direct.EquipExslotShip as D
import Kantour.Core.KcData.Master.Org.Common

data EquipExslotShip = EquipExslotShip
  { shipIds :: IS.IntSet
  , slotItemId :: Int
  }
  deriving (Generic, Show)

instance NFData EquipExslotShip

instance FromDirect EquipExslotShip where
  type Source EquipExslotShip = D.EquipExslotShip

  fromDirect D.EquipExslotShip {shipIds = xs, slotitemId = slotItemId} =
    pure EquipExslotShip {shipIds = IS.fromList xs, slotItemId}

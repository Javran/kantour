{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.EquipShip (
  EquipShip (..),
) where

import qualified Data.IntSet as IS
import qualified Kantour.Core.KcData.Master.Direct.EquipShip as D
import Kantour.Core.KcData.Master.Org.Common

data EquipShip = EquipShip
  { shipId :: Int
  , equipTypes :: IS.IntSet
  }
  deriving (Generic, Show)

instance NFData EquipShip

instance FromDirect EquipShip where
  type Source EquipShip = D.EquipShip

  fromDirect D.EquipShip {shipId, equipType} =
    pure EquipShip{shipId, equipTypes = IS.fromList equipType}

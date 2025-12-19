{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.EquipShip (
  EquipShip (..),
) where

-- TODO: maybe use IS?
-- import qualified Data.IntSet as IS
import qualified Kantour.Core.KcData.Master.Direct.EquipShip as D
import Kantour.Core.KcData.Master.Org.Common
import qualified Data.IntMap.Strict as IM

newtype EquipShip = EquipShip
  { getEquipShip :: IM.IntMap D.EquipShipObj
  }
  deriving (Generic, Show)

instance NFData EquipShip

instance FromDirect EquipShip where
  type Source EquipShip = D.EquipShip

  fromDirect D.EquipShip {getEquipShip = es} =
    pure $ EquipShip es

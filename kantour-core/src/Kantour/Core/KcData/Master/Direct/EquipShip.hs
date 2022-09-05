module Kantour.Core.KcData.Master.Direct.EquipShip (
  EquipShip (..),
) where

import Data.Aeson as Aeson
import Kantour.Core.KcData.Master.Direct.Common

data EquipShip = EquipShip
  { shipId :: Int
  , equipType :: [Int]
  }
  deriving stock (Generic, Show)

instance FromJSON EquipShip where
  parseJSON = parseKcMstJson [("bgmId", "id")]

instance NFData EquipShip
instance HasKnownFields EquipShip where
  knownFields _ =
    kcFields "ship_id equip_type"

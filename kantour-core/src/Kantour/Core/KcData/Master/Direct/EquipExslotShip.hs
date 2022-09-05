module Kantour.Core.KcData.Master.Direct.EquipExslotShip (
  EquipExslotShip (..),
) where

import Data.Aeson as Aeson
import Kantour.Core.KcData.Master.Direct.Common

data EquipExslotShip = EquipExslotShip
  { shipIds :: [Int]
  , slotitemId :: Int
  }
  deriving stock (Generic, Show)

instance FromJSON EquipExslotShip where
  parseJSON = parseKcMstJson

instance NFData EquipExslotShip
instance HasKnownFields EquipExslotShip where
  knownFields _ =
    kcFields "ship_ids slotitem_id"

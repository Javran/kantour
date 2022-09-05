module Kantour.Core.KcData.Master.Direct.EquipShip (
  EquipShip (..),
) where

import Data.Aeson as Aeson
import Deriving.Aeson
import Kantour.Core.KcData.Master.Direct.Common

data EquipShip = EquipShip
  { shipId :: Int
  , equipType :: [Int]
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier KcConvention]
          EquipShip

instance NFData EquipShip
instance HasKnownFields EquipShip where
  knownFields _ =
    kcFields "ship_id equip_type"

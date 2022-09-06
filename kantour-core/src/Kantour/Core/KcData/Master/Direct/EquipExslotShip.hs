module Kantour.Core.KcData.Master.Direct.EquipExslotShip (
  EquipExslotShip (..),
) where

import Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
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

instance Verifiable EquipExslotShip where
  verify EquipExslotShip {shipIds, slotitemId} = do
    case findDuplicates shipIds of
      [] -> pure ()
      xs@(_ : _) -> do
        vLogS $
          "EquipExslotShip{"
            <> show slotitemId
            <> "}: items not unique: "
            <> show (fmap (NE.take 2) xs)

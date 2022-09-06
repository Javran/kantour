{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.EquipShip (
  EquipShip (..),
) where

import Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import Kantour.Core.KcData.Master.Direct.Common

data EquipShip = EquipShip
  { shipId :: Int
  , equipType :: [Int]
  }
  deriving stock (Generic, Show)

instance FromJSON EquipShip where
  parseJSON = parseKcMstJson

instance NFData EquipShip

instance HasKnownFields EquipShip where
  knownFields _ =
    kcFields "ship_id equip_type"

instance Verifiable EquipShip where
  verify EquipShip {shipId, equipType} = do
    case findDuplicates equipType of
      [] -> pure ()
      xs@(_ : _) -> do
        vLogS $
          "EquipShip{"
            <> show shipId
            <> "}: items not unique: "
            <> show (fmap (NE.take 2) xs)

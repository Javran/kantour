{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.EquipShip
  ( EquipTypeValue (..)
  , EquipShipObj (..)
  , EquipShip (..)
  )
where

import Data.Aeson as Aeson
import qualified Data.IntMap.Strict as IM
import qualified Data.List.NonEmpty as NE
import Kantour.Core.KcData.Master.Direct.Common

newtype EquipTypeValue = EquipTypeValue (Maybe (NE.NonEmpty Int))
  deriving stock (Generic, Show)

instance NFData EquipTypeValue

instance FromJSON EquipTypeValue where
  parseJSON = \case
    Null -> pure (EquipTypeValue Nothing)
    ar -> do
      xs <- parseJSON @(NE.NonEmpty Int) ar
      pure (EquipTypeValue (Just xs))

data EquipShipObj = EquipShipObj
  { equipType :: IM.IntMap EquipTypeValue
  }
  deriving stock (Generic, Show)

instance NFData EquipShipObj

instance HasKnownFields EquipShipObj where
  knownFields _ = kcFields "equip_type"

instance FromJSON EquipShipObj where
  parseJSON = withObject "EquipShipObj" $ \v -> do
    (obj :: IM.IntMap EquipTypeValue) <- v .: "api_equip_type"
    pure $ EquipShipObj obj

newtype EquipShip = EquipShip
  { getEquipShip :: IM.IntMap EquipShipObj
  }
  deriving stock (Generic, Show)

instance FromJSON EquipShip where
  parseJSON = parseKcMstJson

instance NFData EquipShip

instance HasKnownFields EquipShip where
  knownFields _ =
    kcFields "ship_id equip_type"

instance Verifiable EquipShip where
  verify EquipShip {} = do
    -- TODO: verification
    pure ()

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Kantour.Core.KcData.Master.Direct.EquipExslotShip (
  EquipExslotShip (..),
  EquipExslotShipInfoF (..),
  EquipExslotShipInfo,
) where

import Data.Aeson as Aeson
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AK
import Data.Coerce (coerce)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import Kantour.Core.KcData.Master.Direct.Common

newtype EquipExslotShip = EquipExslotShip
  { getEquipExslotShip :: IM.IntMap EquipExslotShipInfo
  }
  deriving stock (Generic, Show)

instance FromJSON EquipExslotShip where
  parseJSON = withObject "EquipExslotShip" $ \o ->
    EquipExslotShip <$> do
      let parsePair (rawK, rawV) = do
            let rawK' = AK.toText rawK
            guard $ isIntParsable rawK'
            v <- parseJSON rawV
            pure (read @Int (T.unpack rawK'), v)
      xs :: [(Int, EquipExslotShipInfo)] <- mapM parsePair (AK.toList o)
      pure $ IM.fromList xs

instance NFData EquipExslotShip

instance HasKnownFields EquipExslotShip where
  knownFields _ =
    kcFields "ship_ids slotitem_id"

instance Verifiable EquipExslotShip where
  -- TODO: verification
  verify _ = pure ()

newtype IntMapByObj v = IntMapByObj (IM.IntMap v)
  deriving stock (Generic, Show)
  deriving newtype (NFData)

instance FromJSON v => FromJSON (IntMapByObj v) where
  parseJSON = withObject "IntMapByObj" $ \o ->
    IntMapByObj <$> do
      let parsePair (rawK, rawV) = do
            let rawK' = AK.toText rawK
            guard $ isIntParsable rawK'
            v <- parseJSON @v rawV
            pure (read @Int (T.unpack rawK'), v)
      xs <- mapM parsePair (AK.toList o)
      pure $ IM.fromList xs

-- Information object for a slotitem.

data EquipExslotShipInfoF f = EquipExslotShipInfo
  { shipIds :: Maybe (f Int)
  , ctypes :: Maybe (f Int)
  , stypes :: Maybe (f Int)
  }
  deriving stock (Generic)

deriving instance Show (f Int) => Show (EquipExslotShipInfoF f)

type EquipExslotShipInfo = EquipExslotShipInfoF IM.IntMap

instance FromJSON EquipExslotShipInfo where
  parseJSON o = do
    v :: EquipExslotShipInfoF IntMapByObj <- parseKcMstJson o
    pure (coerce v :: EquipExslotShipInfo)

instance NFData EquipExslotShipInfo

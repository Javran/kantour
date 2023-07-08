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
  parseJSON o =
    EquipExslotShip <$> do
      IntMapByObj v <- parseJSON @(IntMapByObj EquipExslotShipInfo) o
      pure v

instance NFData EquipExslotShip

instance Verifiable EquipExslotShip where
  verify (EquipExslotShip m) = mapM_ verify m

{-
  Parse an object whose keys are stringified integers into IntMap.

  Note: this could be general-purpose.
 -}
newtype IntMapByObj v = IntMapByObj (IM.IntMap v)
  deriving stock (Generic, Show)
  deriving newtype (NFData)

instance FromJSON v => FromJSON (IntMapByObj v) where
  parseJSON = withObject "IntMapByObj" $ \o ->
    IntMapByObj <$> do
      let parsePair (k0, rawV) = do
            [(k1, "")] <- pure $ reads @Integer (T.unpack (AK.toText k0))
            let k2 = fromInteger @Int k1
            guard $ k1 == toInteger k2
            (k2,) <$> parseJSON @v rawV
      IM.fromList <$> mapM parsePair (AK.toList o)

{-
  Information object for a slotitem.

  Generalized to make it more convenient to handle parsing.
 -}
data EquipExslotShipInfoF f = EquipExslotShipInfo
  { shipIds :: Maybe (f Int)
  , ctypes :: Maybe (f Int)
  , stypes :: Maybe (f Int)
  }
  deriving stock (Generic)

deriving instance Show (f Int) => Show (EquipExslotShipInfoF f)

instance HasKnownFields EquipExslotShipInfo where
  knownFields _ =
    kcFields "ship_ids ctypes stypes"

instance Verifiable EquipExslotShipInfo where
  verify EquipExslotShipInfo {shipIds, ctypes, stypes} = do
    let verify' what = \case
          Nothing -> pure ()
          Just m -> forM_ (IM.toList m) \(k, v) -> when (v /= 1) do
            vLogS $
              "Slotitem " <> show k <> ", field " <> what <> ": expected 1 but found " <> show v
    verify' "shipIds" shipIds
    verify' "ctypes" ctypes
    verify' "stypes" stypes

type EquipExslotShipInfo = EquipExslotShipInfoF IM.IntMap

instance FromJSON EquipExslotShipInfo where
  parseJSON =
    fmap
      ( coerce
          @(EquipExslotShipInfoF IntMapByObj)
          @EquipExslotShipInfo
      )
      . parseKcMstJson

instance NFData EquipExslotShipInfo

{-# LANGUAGE DuplicateRecordFields, NoFieldSelectors #-}
{-# LANGUAGE UndecidableInstances #-}

module Kantour.Core.KcData.Master.Direct.EquipExslotShip
  ( EquipExslotShip (..)
  , EquipExslotShipInfoF (..)
  , EquipExslotShipInfo
  ) where

import Data.Aeson as Aeson
import Data.Coerce (coerce)
import qualified Data.IntMap.Strict as IM
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
  Information object for a slotitem.

  Generalized to make it more convenient to handle parsing.
 -}
data EquipExslotShipInfoF f = EquipExslotShipInfo
  { shipIds :: Maybe (f Int)
  , ctypes :: Maybe (f Int)
  , stypes :: Maybe (f Int)
  , reqLevel :: Int
  }
  deriving stock (Generic)

deriving instance Show (f Int) => Show (EquipExslotShipInfoF f)

instance HasKnownFields EquipExslotShipInfo where
  knownFields _ =
    kcFields "ship_ids ctypes stypes req_level"

instance Verifiable EquipExslotShipInfo where
  verify EquipExslotShipInfo {shipIds, ctypes, stypes, reqLevel} = do
    let verify' what = \case
          Nothing -> pure ()
          Just m -> forM_ (IM.toList m) \(k, v) -> when (v /= 1) do
            vLogS $
              "Slotitem " <> show k <> ", field " <> what <> ": expected 1 but found " <> show v
    verify' "shipIds" shipIds
    verify' "ctypes" ctypes
    verify' "stypes" stypes
    unless (0 <= reqLevel && reqLevel <= 10) do
      vLogS $ "reqLevel should be in 0..10, but found: " <> show reqLevel

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

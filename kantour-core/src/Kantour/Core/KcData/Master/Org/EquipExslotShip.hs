{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.EquipExslotShip (
  EquipExslotShip (..),
) where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS
import qualified Kantour.Core.KcData.Master.Direct.EquipExslotShip as D
import Kantour.Core.KcData.Master.Org.Common

newtype EquipExslotShip = EquipExslotShip
  { getEquipExslotShip :: IM.IntMap EquipExslotShipInfo
  }
  deriving (Generic, Show)

data EquipExslotShipInfo = EquipExslotShipInfo
  { shipIds :: IS.IntSet
  , cTypes :: IS.IntSet
  , sTypes :: IS.IntSet
  }
  deriving (Generic, Show)

instance NFData EquipExslotShipInfo

instance NFData EquipExslotShip

instance FromDirect EquipExslotShip where
  type Source EquipExslotShip = D.EquipExslotShip

  fromDirect (D.EquipExslotShip raw) = EquipExslotShip <$> do traverse convertObj raw
    where
      convertSet = maybe IS.empty IM.keysSet
      convertObj
        D.EquipExslotShipInfo
          { D.shipIds = rawShipIds
          , D.ctypes
          , D.stypes
          } =
          pure
            EquipExslotShipInfo
              { shipIds = convertSet rawShipIds
              , cTypes = convertSet ctypes
              , sTypes = convertSet stypes
              }

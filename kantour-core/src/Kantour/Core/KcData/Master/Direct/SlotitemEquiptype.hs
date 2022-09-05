{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.SlotitemEquiptype (
  SlotitemEquiptype (..),
) where

import qualified Data.Text as T
import Data.Aeson
import Kantour.Core.KcData.Master.Direct.Common

data SlotitemEquiptype = SlotitemEquiptype
  { kcId :: Int
  , showFlg :: Int
  , name :: T.Text
  }
  deriving stock (Generic, Show)

instance FromJSON SlotitemEquiptype where
  parseJSON = parseKcMstJson

instance NFData SlotitemEquiptype

instance HasKnownFields SlotitemEquiptype where
  knownFields _ =
    kcFields
      "id show_flg name"

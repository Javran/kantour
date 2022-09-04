{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Direct.SlotitemEquiptype (
  SlotitemEquiptype (..),
) where

import Control.DeepSeq (NFData)
import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Direct.Common

data SlotitemEquiptype = SlotitemEquiptype
  { sId :: Int
  , showFlg :: Int
  , name :: T.Text
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[ FieldLabelModifier
              ( Rename "sId" "id" : KcConvention
              )
           ]
          SlotitemEquiptype
instance NFData SlotitemEquiptype
instance HasKnownFields SlotitemEquiptype where
  knownFields _ =
    kcFields
      "id show_flg name"

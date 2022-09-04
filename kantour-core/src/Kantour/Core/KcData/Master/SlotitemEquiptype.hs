{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.SlotitemEquiptype (
  SlotitemEquiptype (..),
) where

import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Common

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

instance HasKnownFields SlotitemEquiptype where
  knownFields _ =
    kcFields
      "id show_flg name"

{-# LANGUAGE DuplicateRecordFields #-}
module Kantour.Core.KcData.Master.Org.EquipCategory
  ( EquipCategory(..)
    )
  where

import qualified Data.Text as T
import qualified Kantour.Core.KcData.Master.Direct.SlotitemEquiptype as D
import Kantour.Core.KcData.Master.Org.Common

data EquipCategory = EquipCategory
  { kcId :: Int
  , showFlag :: Bool
  , name :: T.Text
  }
  deriving (Generic, Show)

instance NFData EquipCategory

instance FromDirect EquipCategory where
  type Source EquipCategory = D.SlotitemEquiptype

  fromDirect D.SlotitemEquiptype {kcId, showFlg, name} = do
    showFlag <- intBoolFlag "showFlg" showFlg
    pure EquipCategory {kcId, showFlag, name}


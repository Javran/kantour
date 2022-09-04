{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Direct.Stype (
  Stype (..),
) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Direct.Common

data Stype = Stype
  { kcnt :: Int
  , sortno :: Int
  , scnt :: Int
  , equipType :: HM.HashMap T.Text Int
  , name :: T.Text
  , sId :: Int
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[ FieldLabelModifier (Rename "sId" "id" : KcConvention)
           ]
          Stype

instance HasKnownFields Stype where
  knownFields _ =
    kcFields
      "kcnt sortno scnt equip_type name id"

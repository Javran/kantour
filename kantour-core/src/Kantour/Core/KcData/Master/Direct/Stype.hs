{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.Stype (
  Stype (..),
) where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
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

instance FromJSON Stype where
  parseJSON = parseKcMstJson [("sId", "id")]

instance NFData Stype

instance HasKnownFields Stype where
  knownFields _ =
    kcFields
      "kcnt sortno scnt equip_type name id"

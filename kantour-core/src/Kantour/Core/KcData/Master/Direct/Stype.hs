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
  , kcId :: Int
  }
  deriving stock (Generic, Show)

instance FromJSON Stype where
  parseJSON = parseKcMstJson

instance NFData Stype

instance HasKnownFields Stype where
  knownFields _ =
    kcFields
      "kcnt sortno scnt equip_type name id"

instance Verifiable Stype where
  verify Stype {kcId, name, equipType} = do
    let warn msg =
          vLogS $
            "Stype{" <> T.unpack name <> "," <> show kcId <> "}: " <> msg
    unless (all isIntParsable (HM.keys equipType)) do
      warn "equipType: some keys are not parsable as int"
    unless (all (inRange (0,1)) (HM.elems equipType)) do
      warn "equipType: some values are outside [0,1]"

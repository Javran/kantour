{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Direct.Mapinfo (
  Mapinfo (..),
) where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Direct.Common

data Mapinfo = Mapinfo
  { infotext :: T.Text
  , sallyFlag :: [Int]
  , requiredDefeatCount :: Maybe Int
  , mId :: Int
  , item :: [Int]
  , maxMaphp :: Maybe Int
  , opetext :: T.Text
  , name :: T.Text
  , mapareaId :: Int
  , no :: Int
  , level :: Int
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[ FieldLabelModifier
              ( Rename "mId" "id" : KcConvention
              )
           ]
          Mapinfo

instance HasKnownFields Mapinfo where
  knownFields _ =
    kcFields
      "infotext sally_flag required_defeat_count id item max_maphp \
      \opetext name maparea_id no level"

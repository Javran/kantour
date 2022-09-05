{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.Furniture (
  Furniture (..),
) where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Kantour.Core.KcData.Master.Direct.Common

data Furniture = Furniture
  { title :: T.Text
  , price :: Int
  , season :: Int
  , version :: Int
  , activeFlag :: Int
  , outsideId :: Int
  , fType :: Int
  , rarity :: Int
  , fId :: Int
  , no :: Int
  , description :: T.Text
  , saleflg :: Int
  }
  deriving stock (Generic, Show)

instance FromJSON Furniture where
  parseJSON = parseKcMstJson [("fType", "type"), ("fId", "id")]

instance NFData Furniture
instance HasKnownFields Furniture where
  knownFields _ =
    kcFields
      "title price season version active_flag outside_id \
      \type rarity id no description saleflg"

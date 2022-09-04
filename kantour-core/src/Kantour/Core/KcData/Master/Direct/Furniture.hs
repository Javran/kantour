{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Direct.Furniture (
  Furniture (..),
) where

import Control.DeepSeq (NFData)
import Data.Aeson as Aeson
import qualified Data.Text as T
import Deriving.Aeson
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
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier (Rename "fType" "type" : Rename "fId" "id" : KcConvention)]
          Furniture

instance NFData Furniture
instance HasKnownFields Furniture where
  knownFields _ =
    kcFields
      "title price season version active_flag outside_id \
      \type rarity id no description saleflg"

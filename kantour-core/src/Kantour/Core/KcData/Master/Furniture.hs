{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Kantour.Core.KcData.Master.Furniture
  ( Furniture (..)
  )
where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Common

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

instance HasKnownFields Furniture where
  knownFields _ =
    kcFields
      "title price season version active_flag outside_id \
      \type rarity id no description saleflg"

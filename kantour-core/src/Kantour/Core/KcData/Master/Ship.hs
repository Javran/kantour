{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Ship
  ( Ship (..)
  )
where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Common

data Ship = Ship
  { shipId :: Int
  , ctype :: Int
  , name :: T.Text
  , slotNum :: Int
  , sortId :: Int
  , tyku :: Maybe [Int]
  , buildtime :: Maybe Int
  , leng :: Maybe Int
  , backs :: Maybe Int
  , voicef :: Maybe Int
  , houg :: Maybe [Int]
  , getmes :: Maybe T.Text
  , bullMax :: Maybe Int
  , fuelMax :: Maybe Int
  , luck :: Maybe [Int]
  , afterfuel :: Maybe Int
  , aftershipid :: Maybe T.Text
  , afterbull :: Maybe Int
  , sortno :: Maybe Int
  , yomi :: T.Text
  , souk :: Maybe [Int]
  , broken :: Maybe [Int]
  , taik :: Maybe [Int]
  , afterlv :: Maybe Int
  , soku :: Maybe Int
  , stype :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier (Rename "shipId" "id" : KcConvention)]
          Ship

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Slotitem
  ( Slotitem (..)
  )
where

import Deriving.Aeson
import Kantour.Core.KcData.Master.Common
import qualified Data.Text as T

data Slotitem = Slotitem
  { slotId :: Int
  , sakb :: Int
  , bakk :: Int
  , tyku :: Int
  , leng :: Int
  , houg :: Int
  , version :: Maybe Int
  , tais :: Int
  , houm :: Int
  , saku :: Int
  , luck :: Int
  , raik :: Int
  , sortno :: Int
  , sType :: [Int]
  , raig :: Int
  , baku :: Int
  , soku :: Int
  , souk :: Int
  , broken :: [Int]
  , taik :: Int
  , raim :: Int
  , name :: T.Text
  , usebull :: T.Text
  , atap :: Int
  , rare :: Int
  , houk :: Int
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier (
               Rename "slotId" "id" : Rename "sType" "type" : KcConvention)]
          Slotitem

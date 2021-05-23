{-# LANGUAGE DeriveGeneric #-}

module Kantour.KcData.Map.Spot where

import GHC.Generics
import Data.Aeson
import qualified Data.Text as T

data Spot = Spot
  { no :: Int
  , x :: Int
  , y :: Int
  , direction :: Maybe T.Text
  , line :: Maybe Value
  , cpoint :: Maybe Value
  , color :: Maybe Int
  , offsets :: Maybe Value
  , branch :: Maybe Value
  , route :: Maybe Value
  , ration :: Maybe Value
  , landing :: Maybe Value
  , replenish :: Maybe Value
  , repair :: Maybe Value
  }
  deriving (Generic)

instance FromJSON Spot

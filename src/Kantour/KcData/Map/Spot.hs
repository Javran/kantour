{-# LANGUAGE DeriveGeneric #-}

module Kantour.KcData.Map.Spot where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Kantour.KcData.Map.Xywh
import qualified Data.HashMap.Strict as HM

data Spot = Spot
  { no :: Int
  , x :: Int
  , y :: Int
  , direction :: Maybe T.Text
  , line :: Maybe (Xy Int)
  , cpoint :: Maybe Value
  , color :: Maybe Int
  , offsets :: Maybe (HM.HashMap T.Text (Xy Int))
  , branch :: Maybe Value
  , route :: Maybe Value
  , ration :: Maybe Value
  , landing :: Maybe Value
  , replenish :: Maybe Value
  , repair :: Maybe Value
  }
  deriving (Generic, Show)

instance FromJSON Spot

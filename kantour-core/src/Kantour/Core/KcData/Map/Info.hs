{-# LANGUAGE DeriveGeneric #-}

module Kantour.Core.KcData.Map.Info where

import Data.Aeson
import qualified Data.Vector as V
import GHC.Generics
import Kantour.Core.KcData.Map.Background
import Kantour.Core.KcData.Map.Enemy
import Kantour.Core.KcData.Map.Spot

data Info = Info
  { spots :: V.Vector Spot
  , -- | info{num} meta doesn't seem to have this one.
    bg :: Maybe (V.Vector Background)
  , enemies :: Maybe (V.Vector Enemy)
  , recce :: Maybe (V.Vector Value)
  , airbase :: Maybe Value
  , airbaseraid :: Maybe Value
  , airraids :: Maybe (V.Vector Value)
  , labels :: Maybe Value
  }
  deriving (Generic, Show)

instance FromJSON Info

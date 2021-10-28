{-# LANGUAGE DeriveGeneric #-}

module Kantour.KcData.Map.Enemy where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data Enemy = Enemy
  { no :: Int
  , x :: Int
  , y :: Int
  , img :: T.Text
  }
  deriving (Generic, Show)

instance FromJSON Enemy

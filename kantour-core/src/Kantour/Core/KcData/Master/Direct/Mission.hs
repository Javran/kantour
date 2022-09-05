{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.Mission (
  Mission (..),
) where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Kantour.Core.KcData.Master.Direct.Common

data Mission = Mission
  { winItem1 :: [Int]
  , useBull :: Double
  , dispNo :: T.Text
  , time :: Int
  , resetType :: Int
  , details :: T.Text
  , sampleFleet :: [Int]
  , damageType :: Int
  , returnFlag :: Int
  , winMatLevel :: [Int]
  , difficulty :: Int
  , deckNum :: Int
  , winItem2 :: [Int]
  , mId :: Int
  , name :: T.Text
  , mapareaId :: Int
  , useFuel :: Double
  }
  deriving stock (Generic, Show)

instance FromJSON Mission where
  parseJSON = parseKcMstJson [("mId", "id")]

instance NFData Mission

instance HasKnownFields Mission where
  knownFields _ =
    kcFields
      "win_item1 use_bull disp_no time reset_type details sample_fleet \
      \damage_type return_flag win_mat_level difficulty deck_num win_item2 \
      \id name maparea_id use_fuel"

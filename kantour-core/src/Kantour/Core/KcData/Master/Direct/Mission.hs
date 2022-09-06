{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.Mission (
  Mission (..),
) where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Kantour.Core.KcData.Master.Direct.Common

data Mission = Mission
  { kcId :: Int
  , damageType :: Int
  , deckNum :: Int
  , details :: T.Text
  , difficulty :: Int
  , dispNo :: T.Text
  , mapareaId :: Int
  , name :: T.Text
  , resetType :: Int
  , returnFlag :: Int
  , sampleFleet :: [Int]
  , time :: Int
  , useBull :: Double
  , useFuel :: Double
  , winItem1 :: [Int]
  , winItem2 :: [Int]
  , winMatLevel :: [Int]
  }
  deriving stock (Generic, Show)

instance FromJSON Mission where
  parseJSON = parseKcMstJson

instance NFData Mission

instance HasKnownFields Mission where
  knownFields _ =
    kcFields
      "win_item1 use_bull disp_no time reset_type details sample_fleet \
      \damage_type return_flag win_mat_level difficulty deck_num win_item2 \
      \id name maparea_id use_fuel"

instance Verifiable Mission where
  verify Mission{kcId, winItem1, winItem2} = do
    let warn msg = vLogS $ "Mission{" <> show kcId <> "}: " <> msg
    case winItem1 of
      [_, _] -> pure ()
      xs -> warn $ "winItem1 expect exactly 2:" <> show xs
    case winItem2 of
      [_, _] -> pure ()
      xs -> warn $ "winItem1 expect exactly 2:" <> show xs

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Kantour.WhoCallsTheFleet.Types.Equipment where

import GHC.Generics
import Data.Aeson
import Control.Applicative
import Kantour.WhoCallsTheFleet.Types.Common

data Equipment = Equipment
  { masterId :: Int
  , rarity :: Int
  , eqpType :: Int
  , name :: Name
  , stat :: EquipmentStat
  , dismantle :: Dismantle
  , craftable :: Bool
  , improvable :: Bool
  , rankUpgradable :: ()
  } deriving (Generic, Show)

data EquipmentStat = EquipmentStat
  { fire :: Int
  , torpedo :: Int
  , bomb :: Int
  , antiSub :: Int
  , antiAir :: Int
  , armor :: Int
  , evasion :: Int
  , hit :: Int
  , lineOfSight :: Int
  } deriving (Generic, Show)

type Dismantle = Resource Int

instance FromJSON Equipment where
    parseJSON = withObject "Equipment" $ \v -> Equipment
        <$> v .: "id"
        <*> v .: "rarity"
        <*> v .: "type"
        <*> v .: "name"
        <*> v .: "stat"
        <*> v .: "dismantle"
        <*> (v .: "craftable" <|> pure False)
        <*> (v .: "improvable" <|> pure False)
        <*> pure ()

instance FromJSON EquipmentStat where
    parseJSON = withObject "EquipmentStat" $ \v -> EquipmentStat
        <$> v .: "fire"
        <*> v .: "torpedo"
        <*> v .: "bomb"
        <*> v .: "asw"
        <*> v .: "aa"
        <*> v .: "armor"
        <*> v .: "evasion"
        <*> v .: "hit"
        <*> v .: "los"

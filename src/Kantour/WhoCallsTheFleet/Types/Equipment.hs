{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Kantour.WhoCallsTheFleet.Types.Equipment where

import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Applicative
import Control.Monad

data Equipment = Equipment
  { masterId :: Int
  , rarity :: Int
  , eqpType :: Int
  , name :: EquipmentName
  , stat :: EquipmentStat
  , dismantle :: EquipmentDismantle
  , craftable :: Bool
  , improvable :: Bool
  , rankUpgradable :: ()
  } deriving (Generic, Show)

data EquipmentName = EquipmentName
  { jaJP :: T.Text
  , jaKana :: T.Text
  , jaRomaji :: T.Text
  , zhCN :: T.Text
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

data EquipmentDismantle = EquipmentDismantle
  { fuel :: Int
  , ammo :: Int
  , steel :: Int
  , bauxite :: Int
  } deriving (Generic, Show)

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

instance FromJSON EquipmentName where
    parseJSON = withObject "EquipmentName" $ \v -> EquipmentName
        <$> v .: "ja_jp"
        <*> v .: "ja_kana"
        <*> v .: "ja_romaji"
        <*> v .: "zh_cn"

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

instance FromJSON EquipmentDismantle where
    parseJSON = withArray "EquipmentDismantle" $ \arr -> do
        guard $ V.length arr == 4
        EquipmentDismantle
            <$> parseJSON (arr V.! 0)
            <*> parseJSON (arr V.! 1)
            <*> parseJSON (arr V.! 2)
            <*> parseJSON (arr V.! 3)

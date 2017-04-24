{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Kantour.WhoCallsTheFleet.Types.Equipment where

import GHC.Generics
import Data.Aeson
import qualified Data.Text as T

data Equipment = Equipment
  { masterId :: Int
  , rarity :: Int
  , eqpType :: Int
  , name :: EquipmentName
  , stat :: ()
  , dismantle :: ()
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

instance FromJSON Equipment where
    parseJSON = withObject "Equipment" $ \v -> Equipment
        <$> v .: "id"
        <*> v .: "rarity"
        <*> v .: "type"
        <*> v .: "name"
        <*> pure ()
        <*> pure ()
        <*> v .: "craftable"
        <*> v .: "improvable"
        <*> pure ()

instance FromJSON EquipmentName where
    parseJSON = withObject "EquipmentName" $ \v -> EquipmentName
        <$> v .: "ja_jp"
        <*> v .: "ja_kana"
        <*> v .: "ja_romaji"
        <*> v .: "zh_cn"

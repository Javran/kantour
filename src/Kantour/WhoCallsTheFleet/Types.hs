{-# LANGUAGE
    DeriveGeneric
  , OverloadedStrings
  , DuplicateRecordFields
  #-}
module Kantour.WhoCallsTheFleet.Types where

import Data.Aeson
import Data.Aeson.Types
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Text as T
import GHC.Generics
import Data.Semigroup

-- https://github.com/Diablohu/KanColle-JSON-Database/wiki/ships.json

type MasterId = Int

data Ship = Ship
  { masterId :: MasterId
  , libraryId :: Int
  , name :: ShipName
  , stat :: ShipStat
  , remodel :: Maybe RemodelInfo
  , scrap :: Maybe Scrap
  , shipTypeId :: Int
  , classId :: Value
  , classDesination :: Maybe Int
  , seriesId :: Maybe Int
  , baseLevel :: Int
  , buildTime :: Maybe Int
  , rarity :: Maybe Int
  , consumption :: Consumption
  } deriving (Generic, Show)

data ShipName = ShipName
  { jaJP :: T.Text
  , jaKana :: T.Text
  , jaRomaji :: T.Text
  , zhCN :: T.Text
  , suffix :: Maybe Int
  } deriving (Generic, Show)

data ShipStat = ShipStat
  { fire :: StatRange Int
  , torpedo :: StatRange Int
  , antiAir :: StatRange Int
  , antiSub :: StatRange Int
  , hp :: StatRange Int
  , armor :: StatRange Int
  , evasion :: StatRange Int
  , lineOfSight :: StatRange Int
  , luck :: StatRange Int
  , carry :: Int
  , speed :: Int
  , range :: Int

  } deriving (Generic, Show)

data StatRange a = StatRange
  { base :: a
  , max :: a
  } deriving (Generic, Show)

data RemodelInfo = RemodelInfo
  { prev :: Maybe MasterId
  , next :: Maybe MasterId
  , nextLevel :: Maybe Int
  , remodelLoop :: Maybe Bool
  } deriving (Generic, Show)

data Scrap = Scrap
  { fuel :: Int
  , ammo :: Int
  , steel :: Int
  , bauxite :: Int
  } deriving (Generic, Show)

data Consumption = Consumption
  { fuel :: Int
  , ammo :: Int
  } deriving (Generic, Show)

{-

TODO

* `slot` 装备格 & 每格搭载
  * 元素数量表示有多少装备格
  * 每个数字表示该格搭载量

* `equip` 默认装备
  * 对应装备格

* `lines` 台词
  * `start` 入手台词

* `modernization` 近代化改修（合成）所得属性
  * 按顺序资源为：火力、雷装、对空、装甲、运

* `remodel_cost` 改装消耗
  * `ammo` 弹耗
  * `steel` 钢耗

* `additional_item_types` 额外可装备类型ID -> `item_types.json`

* `rels` 相关信息
  * `cv` 声优ID -> `entities.json`
  * `illustrator` 画师ID -> `entities.json`

* `links` 相关链接
  * `name` 链接名
  * `url` 链接地址
  -}

parseRange :: FromJSON a => T.Text -> Object -> Parser (StatRange a)
parseRange fieldName v = StatRange
    <$> v .: fieldName
    <*> v .: fieldNameMax
  where
    fieldNameMax = fieldName <> "_max"

instance FromJSON Ship where
    parseJSON = withObject "Ship" $ \v -> Ship
        <$> v .: "id"
        <*> v .: "no"
        <*> v .: "name"
        <*> v .: "stat"
        <*> v .:? "remodel"
        <*> v .:? "scrap"
        <*> v .: "type"
        <*> v .: "class"
        <*> v .:? "class_no"
        <*> v .:? "series"
        <*> v .: "base_lvl"
        <*> v .:? "buildtime"
        <*> v .:? "rare"
        <*> v .: "consum"

instance FromJSON ShipName where
    parseJSON = withObject "ShipName" $ \v -> ShipName
        <$> v .: "ja_jp"
        <*> v .: "ja_kana"
        <*> v .: "ja_romaji"
        <*> v .: "zh_cn"
        <*> v .:? "suffix"

instance FromJSON ShipStat where
    parseJSON = withObject "ShipStat" $ \v -> ShipStat
        <$> parseRange "fire" v
        <*> parseRange "torpedo" v
        <*> parseRange "aa" v
        <*> parseRange "asw" v
        <*> parseRange "hp" v
        <*> parseRange "armor" v
        <*> parseRange "evasion" v
        <*> parseRange "los" v
        <*> parseRange "luck" v
        <*> v .: "carry"
        <*> v .: "speed"
        <*> v .: "range"

instance FromJSON RemodelInfo where
    parseJSON = withObject "RemodelInfo" $ \v -> RemodelInfo
        <$> v .:? "prev"
        <*> v .:? "next"
        <*> v .:? "next_lvl"
        <*> v .:? "loop"

instance FromJSON Scrap where
    parseJSON = withArray "Scrap" $ \arr -> do
        guard $ V.length arr == 4
        Scrap
            <$> parseJSON (arr V.! 0)
            <*> parseJSON (arr V.! 1)
            <*> parseJSON (arr V.! 2)
            <*> parseJSON (arr V.! 3)

instance FromJSON Consumption where
    parseJSON = withObject "Consumption" $ \v -> Consumption
        <$> v .: "fuel"
        <*> v .: "ammo"

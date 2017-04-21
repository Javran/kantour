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
import Control.Applicative

-- https://github.com/Diablohu/KanColle-JSON-Database/wiki/ships.json

type MasterId = Int

-- perhaps a bad idea, but let's try this anyway.
-- a better alternative would be to provide "safe accessors" depending
-- on master id
-- masterId: 9181, 9182, 9183
data BlueSteelShip = BlueSteelShip
  { masterId :: MasterId
  , raw :: Value
  } deriving (Generic, Show)

data Ship = Ship
  { masterId :: MasterId
  , libraryId :: Int
  , name :: ShipName
  , stat :: ShipStat
  , remodel :: Maybe RemodelInfo
  , remodelCost :: Maybe RemodelInfo
  , scrap :: Maybe Scrap
  , shipTypeId :: Int
  , classId :: Value
  , classDesination :: Maybe Int
  , seriesId :: Maybe Int
  , baseLevel :: Int
  , buildTime :: Maybe Int
  , rarity :: Maybe Int
  , consumption :: Consumption
  , modernization :: Maybe Modernization
  , slots :: Array
  , equips :: Maybe Array
  , lines :: Maybe Value
  , rels :: Maybe Value
  , links :: Maybe Value
  , additionalItemTypes :: Maybe [Int]
  , raw :: Value
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

data RemodelCost = RemodelCost
  { ammo :: Int
  , steel :: Int
  } deriving (Generic, Show)

data Modernization = Modernization
  { fire :: Int
  , torpedo :: Int
  , antiAir :: Int
  , armor :: Int
  , luck :: Int -- TODO: doesn't seem to have this field
  } deriving (Generic, Show)

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
        <*> v .:? "remodel_cost"
        <*> v .:? "scrap"
        <*> v .: "type"
        <*> v .: "class"
        <*> v .:? "class_no"
        <*> v .:? "series"
        <*> v .: "base_lvl"
        <*> v .:? "buildtime"
        <*> v .:? "rare"
        <*> v .: "consum"
        <*> v .:? "modernization"
        <*> v .: "slot"
        <*> (equip1 v <|> equipEmpty v)
        <*> v .:? "lines"
        <*> v .:? "rels"
        <*> v .:? "links"
        <*> v .:? "additional_item_types"
        <*> pure (Object v)
      where
        equip1, equipEmpty :: Object -> Parser (Maybe Array)
        equip1 v' = v' .:? "equip"
        equipEmpty v' = do
            Just s <- strP
            guard $ s == ""
            pure Nothing
          where
            strP = v' .:? "equip" :: Parser (Maybe String)

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

instance FromJSON Modernization where
    parseJSON = withArray "Modernization" $ \arr -> do
        guard $ V.length arr == 4
        Modernization
            <$> parseJSON (arr V.! 0)
            <*> parseJSON (arr V.! 1)
            <*> parseJSON (arr V.! 2)
            <*> parseJSON (arr V.! 3)
            <*> pure 0

instance FromJSON BlueSteelShip where
    parseJSON = withObject "BlueSteelShip" $ \v -> do
        mstId <- v .: "masterId" :: Parser Int
        guard $ mstId `elem` [9181, 9182, 9183]
        BlueSteelShip mstId <$> pure (Object v)

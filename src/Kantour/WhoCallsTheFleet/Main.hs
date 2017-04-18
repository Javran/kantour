{-# LANGUAGE OverloadedStrings, DeriveGeneric, TypeFamilies, DuplicateRecordFields #-}
module Kantour.WhoCallsTheFleet.Main where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Data.Aeson
import Data.Char
import Data.MonoTraversable

import GHC.Generics
import Data.String
import Control.Monad
import Data.Word
import Data.Monoid

w2c :: Word8 -> Char
w2c = chr . fromIntegral

repoBase :: String
repoBase = "https://raw.githubusercontent.com/Diablohu/WhoCallsTheFleet/master/app-db/"

fetchURL :: String -> IO LBS.ByteString
fetchURL url = do
    mgr <- getGlobalManager
    req <- parseRequest url
    resp <- httpLbs req mgr
    let st = responseStatus resp
    if st == ok200
        then pure (responseBody resp)
        else fail $ "error with status code: " ++ show (statusCode st)

splitLines :: LBS.ByteString -> [BS.ByteString]
splitLines = filter nonEmptyLine . BSC.split '\n' . LBS.toStrict
  where
    nonEmptyLine = oany (not . isSpace . w2c)

fetchShipsRaw :: IO LBS.ByteString
fetchShipsRaw = fetchURL (repoBase ++ "ships.json")

parseShip :: String -> Maybe Ship
parseShip xs = decode (fromString xs)

defaultMain :: IO ()
defaultMain = do
    raws <- splitLines <$> fetchShipsRaw
    let process :: BSC.ByteString -> IO ()
        process raw = do
            BSC.putStrLn raw
            print (decodeStrict' raw :: Maybe Ship)
            putStrLn "===="
            pure ()
    mapM_ process raws

data Equipment = Equipment
  { masterId :: Int
  , eqpRarity :: Int
  , eqpType :: Int
  , eqpName :: () -- TODO
  , eqpStat :: ()
  , eqpDismantle :: ()
  , eqpCraftable :: Bool
  , eqpImprovable :: Bool
  , eqpRankUpgradable :: Bool
  }

data Ship = Ship
  { masterId :: Int
  , name :: ShipName
  , stat :: ShipStat
  } deriving (Generic, Show)

data ShipName = ShipName
  { jaJP :: T.Text
  , jaKana :: T.Text
  , jaRomaji :: T.Text
  , zhCN :: T.Text
  } deriving (Generic, Show)

data ShipStat = ShipStat
  { fire :: ShipStatRange Int
  , torpedo :: ShipStatRange Int
  , antiAir :: ShipStatRange Int
  , antiSub :: ShipStatRange Int
  , hp :: ShipStatRange Int
  , armor :: ShipStatRange Int
  , evasion :: ShipStatRange Int
  , lineOfSight :: ShipStatRange Int
  , luck :: ShipStatRange Int
  , carry :: Int
  , speed :: Int
  , range :: Int
  } deriving (Generic, Show)

data ShipStatRange a = ShipStatRange
  { base :: a
  , max :: a
  } deriving (Generic, Show)

instance FromJSON Ship where
    parseJSON = withObject "Ship" $ \v -> Ship
        <$> v .: "id"
        <*> v .: "name"
        <*> v .: "stat"

instance FromJSON ShipName where
    parseJSON = withObject "ShipName" $ \v -> ShipName
        <$> v .: "ja_jp"
        <*> v .: "ja_kana"
        <*> v .: "ja_romaji"
        <*> v .: "zh_cn"

instance FromJSON ShipStat where
    parseJSON = withObject "ShipStat" $ \v ->
        ShipStat
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
      where
        parseRange fieldName v = ShipStatRange
            <$> v .: fieldName
            <*> v .: fieldNameMax
          where
            fieldNameMax = fieldName <> "_max"

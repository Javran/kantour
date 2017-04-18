{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
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

import GHC.Generics
import Data.String

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
splitLines = BSC.split '\n' . LBS.toStrict

fetchShipsRaw :: IO LBS.ByteString
fetchShipsRaw = fetchURL (repoBase ++ "ships.json")

parseShip :: String -> Maybe Ship
parseShip xs = decode (fromString xs)

defaultMain :: IO ()
defaultMain = do
    raws <- splitLines <$> fetchShipsRaw
    let process :: BS.ByteString -> IO ()
        process raw = do
            print (decodeStrict' raw :: Maybe Value)
            putStrLn "===="
            pure ()
    mapM_ process raws

data Equipment = Equipment
  { eqpId :: Int
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
  { shipId :: Int
  } deriving (Generic, Show)

instance FromJSON Ship where
    parseJSON = withObject "Ship" $ \v -> Ship
        <$> v .: "id"

{-# LANGUAGE OverloadedStrings, DeriveGeneric, TypeFamilies #-}
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
    nonEmptyLine xs = oany (not . isSpace . w2c) xs

fetchShipsRaw :: IO LBS.ByteString
fetchShipsRaw = fetchURL (repoBase ++ "ships.json")

parseShip :: String -> Maybe Ship
parseShip xs = decode (fromString xs)

defaultMain :: IO ()
defaultMain = do
    raws <- splitLines <$> fetchShipsRaw
    let process :: BSC.ByteString -> IO ()
        process raw = do
            BS.putStrLn raw
            print (decodeStrict' raw :: Maybe Ship)
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

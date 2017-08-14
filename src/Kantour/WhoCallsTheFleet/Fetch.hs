{-# LANGUAGE ScopedTypeVariables #-}
module Kantour.WhoCallsTheFleet.Fetch where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Data.Word
import Data.Char
import Data.MonoTraversable

import Kantour.WhoCallsTheFleet.Types
import Data.Aeson

import Data.Semigroup
import Data.String
import Control.Monad
import Data.Maybe

w2c :: Word8 -> Char
w2c = chr . fromIntegral

splitLines :: LBS.ByteString -> [BS.ByteString]
splitLines = filter nonEmptyLine . BSC.split '\n' . LBS.toStrict
  where
    nonEmptyLine = oany (not . isSpace . w2c)

repoBase :: String
repoBase = "https://raw.githubusercontent.com/Diablohu/KanColle-JSON-Database/master/db/"

fetchURL :: String -> IO LBS.ByteString
fetchURL url = do
    mgr <- getGlobalManager
    req <- parseRequest url
    resp <- httpLbs req mgr
    let st = responseStatus resp
    if st == ok200
        then pure (responseBody resp)
        else fail $ "error with status code: " ++ show (statusCode st)

fetchShipsRaw :: IO [BS.ByteString]
fetchShipsRaw = splitLines <$> fetchURL (repoBase ++ "ships.json")

fetchShips :: IO [Ship]
fetchShips = do
    raws <- fetchShipsRaw
    catMaybes <$> mapM verboseParse raws

fetchEquipmentsRaw :: IO [BS.ByteString]
fetchEquipmentsRaw = splitLines <$> fetchURL (repoBase ++ "items.json")

verboseParse :: forall a. FromJSON a => BS.ByteString -> IO (Maybe a)
verboseParse raw = do
    let result = eitherDecodeStrict' raw :: Either String a
    -- BSC.putStrLn raw
    case result of
        Right m -> pure (Just m)
        Left msg -> do
            putStrLn $ "parsing failed: " <> msg
            pure Nothing

fetchEquipments :: IO [Equipment]
fetchEquipments = do
    raws <- fetchEquipmentsRaw
    catMaybes <$> mapM verboseParse raws

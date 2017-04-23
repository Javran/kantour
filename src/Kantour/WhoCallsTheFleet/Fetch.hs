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
    let process :: BSC.ByteString -> IO (Maybe Ship)
        process raw = do
            let result = eitherDecodeStrict' raw :: Either String Ship
            case result of
                Right m -> do
                    let mz = modernization (m :: Ship)
                    when (isNothing mz) $
                        BSC.putStrLn raw
                    pure (Just m)
                Left msg -> do
                    putStrLn $ "parsing failed: " <> msg
                    BSC.putStrLn raw
                    pure Nothing
    catMaybes <$> mapM process raws

fetchEquipmentsRaw :: IO [BS.ByteString]
fetchEquipmentsRaw = splitLines <$> fetchURL (repoBase ++ "items.json")

fetchEquipments :: IO [Equipment]
fetchEquipments = do
    raws <- fetchEquipmentsRaw
    let process :: BSC.ByteString -> IO (Maybe Equipment)
        process raw = do
            let result = eitherDecodeStrict' raw :: Either String Equipment
            case result of
                Right m -> do
                    print m
                    pure (Just m)
                Left msg -> do
                    putStrLn $ "parsing failed: " <> msg
                    BSC.putStrLn raw
                    pure Nothing
    -- TODO
    catMaybes <$> mapM process raws

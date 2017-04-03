{-# LANGUAGE OverloadedStrings #-}
module Kantour.WhoCallsTheFleet.Main where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types

repoBase :: String
repoBase = "https://raw.githubusercontent.com/Diablohu/WhoCallsTheFleet/master/app-db/"

fetchURL :: String -> IO String
fetchURL url = do
    mgr <- getGlobalManager
    req <- parseRequest url
    resp <- httpLbs req mgr
    let st = responseStatus resp
    if st == ok200
        then pure (T.unpack . T.decodeUtf8 . LBS.toStrict . responseBody $ resp)
        else fail $ "error with status code: " ++ show (statusCode st)

fetchShipsRaw :: IO String
fetchShipsRaw = fetchURL (repoBase ++ "ships.json")

fetchItemsRaw :: IO String
fetchItemsRaw = fetchURL (repoBase ++ "items.json")

defaultMain :: IO ()
defaultMain = fetchItemsRaw >>= putStrLn

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Kantour.ApiParser where

import Control.Monad
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.List
import qualified Data.Text as T
import Kantour.Core.KcData.Master.Common
import Kantour.Core.KcData.Master.Root
import Kantour.Subcommand
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Text.Encoding
import Shower
import System.Environment
import System.Exit

data SubCmdApiParser

instance Subcommand SubCmdApiParser where
  name _ = "ApiParser"
  main _ = defaultMain

loadFromSource :: Manager -> String -> IO BSL.ByteString
loadFromSource mgr src
  | "http" `isPrefixOf` src = do
    req <- parseRequest src
    responseBody <$> httpLbs req mgr
  | otherwise = BSL.readFile src

defaultMain :: IO ()
defaultMain =
  getArgs >>= \case
    [fileOrUrlSrc] -> do
      mgr <- newManager tlsManagerSettings
      rawJson <- loadFromSource mgr fileOrUrlSrc
      CollectExtra {ceValue = r, ceExtra} <-
        case Aeson.eitherDecode @(CollectExtra MasterRoot) rawJson of
          Left msg -> die ("parse error: " <> msg)
          Right r -> pure r
      putStrLn "Ship sample:"
      printer (head $ mstShip r)
      unless (null ceExtra) $ do
        putStrLn "Following fields are not yet accounted for:"
        forM_ ceExtra $ \(k, v) -> do
          putStrLn $ "- " <> T.unpack k
          putStrLn "Truncated sample: "
          putStrLn (T.unpack $ T.take 200 $ decodeUtf8 $ BSL.toStrict $ encode v)
    _ -> die "<subcmd> [http]<source>"

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Kantour.ApiParser where

import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Kantour.Core.KcData.Master.Root
import Kantour.Subcommand
import Network.HTTP.Client
import Network.HTTP.Client.TLS
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
      parsed <- case Aeson.eitherDecode @MasterRoot rawJson of
        Left msg -> die ("parse error: " <> msg)
        Right r -> pure r
      printer parsed
    _ -> die "<subcmd> [http]<source>"

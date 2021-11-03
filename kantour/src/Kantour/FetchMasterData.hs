{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Kantour.FetchMasterData
  ( SubCmdFetchMasterData
  , fetchMasterData
  , loadFromSource
  )
where

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

data SubCmdFetchMasterData

{-
  TODO: the need of getting and parsing api_start2
  comes up so often that it might worth making into enviroment variables.
 -}

instance Subcommand SubCmdFetchMasterData where
  name _ = "FetchMasterData"
  main _ = defaultMain

{-
  TODO: subcommands can share Managers probably?
 -}

loadFromSource :: Manager -> String -> IO BSL.ByteString
loadFromSource mgr src
  | "http" `isPrefixOf` src = do
    req <- parseRequest src
    responseBody <$> httpLbs req mgr
  | otherwise = BSL.readFile src

fetchMasterData :: Manager -> String -> IO MasterRoot
fetchMasterData mgr src =
  loadFromSource mgr src >>= \rawJson ->
    case Aeson.eitherDecode @MasterRoot rawJson of
      Left msg -> die ("parse error: " <> msg)
      Right r -> pure r

defaultMain :: IO ()
defaultMain =
  getArgs >>= \case
    [fileOrUrlSrc] -> do
      mgr <- newManager tlsManagerSettings
      fetchMasterData mgr fileOrUrlSrc >>= printer
    _ -> die "<subcmd> [http]<source>"

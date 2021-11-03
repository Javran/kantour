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

{-
  TODO: do this later proper.

  Plan:
  - current reference impl is: https://github.com/Javran/subtender/blob/23d6f4d9e03cf81ffa0d070ed2bded46afbd1eae/src/poi/selectors.js#L67-L128
    and we want to re-implement it here
  - explore option on how to loop-break a circular chain (would be some combination of api_sortno, api_sort_id and api_id).
  - backport.

 -}
remodelChainExperiment :: MasterRoot -> IO ()
remodelChainExperiment _ = _todo

defaultMain :: IO ()
defaultMain =
  getArgs >>= \case
    [fileOrUrlSrc] -> do
      mgr <- newManager tlsManagerSettings
      fetchMasterData mgr fileOrUrlSrc >>= remodelChainExperiment
    _ -> die "<subcmd> [http]<source>"

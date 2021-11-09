{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Kantour.Core.KcData.Master.Fetch
  ( DataSource (..)
  , dataSourceP
  , dataSourceFromEnv
  , FileMetadata (..)
  , cacheBaseFromEnv
  , fetchRawFromEnv
  )
where

{-
  TODO: WIP
  Fetches master data (api_start2).

  Environment variables:

  - KANTOUR_CACHE_BASE: serves as base path for the cache.

    + <base>/api_start2.json: the data
    + <base>/api_start2.source.yaml: metadata to indicate where it comes from

      for this yaml file:

      source: one of github:... or url:... below, required.
      commit: required for github source.

    the cache is considered invalid if one of the following condition is met:
    - either file is missing or metadata does not contain required fields
    - source indicated in metadata differs from KANTOUR_MASTER_DATA_SOURCE.
      (note that `stock` and `file:` option should completely bypass caching
      mechanism so they won't trigger anything related to caching invalidation)
    - source is from github, and recorded commit string isn't the same
      as one present on GitHub.

    when cache is valid, data from cache will be loaded, otherwise those files are
    overwritten if KANTOUR_MASTER_DATA_SOURCE is one of `github:` or `url:` upon
    successful retrieve.

  - KANTOUR_MASTER_DATA_SOURCE:
    one of:
    + stock (from data shipped with this lib)
      (no caching)
    + github:kcwiki:kancolle-data:master:api/api_start2.json
      fetches from latest commit as specified
    + url:<url address>
      use this if we want to pin down a specific version somewhere
    + file:<filepath> load it locally, no caching.

  Note: to find latest commit:

  > curl -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/kcwiki/kancolle-data/branches/master | jq '.commit.sha'

 -}

import Control.Monad
import Data.Aeson
import Data.Aeson.Picker
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import Kantour.Core.DataFiles
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Directory
import System.Environment
import System.Exit
import Text.ParserCombinators.ReadP

data DataSource
  = DsStock
  | DsGitHub
      { dsgUser :: String
      , dsgRepo :: String
      , dsgBranch :: String
      , dsgPath :: FilePath
      }
  | DsUrl String
  | DsFile FilePath

instance Show DataSource where
  show = \case
    DsStock -> "stock"
    DsGitHub {dsgUser, dsgRepo, dsgBranch, dsgPath} ->
      intercalate ":" [dsgUser, dsgRepo, dsgBranch, dsgPath]
    DsUrl v -> "url:" <> v
    DsFile v -> "file:" <> v

instance Read DataSource where
  readsPrec _ = readP_to_S dataSourceP

instance FromJSON DataSource where
  parseJSON = withText "DataSource" $ \t -> do
    [(v, "")] <- pure (reads (T.unpack t))
    pure v

instance ToJSON DataSource where
  toJSON = String . T.pack . show

dataSourceP :: ReadP DataSource
dataSourceP =
  (DsStock <$ string "stock")
    <++ (string "github:" *> githubP)
    <++ (string "url:" *> (DsUrl <$> takeAll1))
    <++ (string "file:" *> (DsFile <$> takeAll1))
  where
    takeAll1 = munch1 (const True)

    githubP =
      DsGitHub
        <$> chunk1
        <*> chunk1
        <*> chunk1
        <*> takeAll1
      where
        chunk1 :: ReadP String
        chunk1 = munch1 (/= ':') <* char ':'

dataSourceFromEnv :: IO DataSource
dataSourceFromEnv =
  getEnv "KANTOUR_MASTER_DATA_SOURCE" >>= \raw ->
    case readP_to_S (dataSourceP <* eof) raw of
      [(v, "")] -> pure v
      _ -> die "parse error on data source"

{-
  datatype invariant:
  - source can only be github or url
  - commit required if and only if we have github source
 -}
data FileMetadata = FileMetadata
  { fmSource :: DataSource
  , fmCommit :: Maybe T.Text
  }

instance FromJSON FileMetadata where
  parseJSON = withObject "FileMetadata" $ \o -> do
    v@FileMetadata {fmSource, fmCommit} <-
      FileMetadata <$> o .: "source"
        <*> o .:? "commit"
    case fmSource of
      DsGitHub {} ->
        when (isNothing fmCommit) $
          fail "`commit` field required for `github:` source"
      DsUrl {} ->
        when (isJust fmCommit) $
          fail "`commit` field should not appear for `url:`"
      _ -> fail "only `github:` or `url:` is allowed as file metadata"
    pure v

cacheBaseFromEnv :: IO (Maybe FilePath)
cacheBaseFromEnv =
  lookupEnv "KANTOUR_CACHE_BASE" >>= \case
    Just cacheBase | not (all isSpace cacheBase) -> do
      do
        b <- doesPathExist cacheBase
        unless b $ fail $ cacheBase <> " does not exist."
      do
        b <- doesDirectoryExist cacheBase
        unless b $ fail $ cacheBase <> " is not a directory."
      pure (Just cacheBase)
    _ -> pure Nothing

{-
  TODO:

  - ignore cache for now
  - implement caching

 -}
fetchRawFromEnv :: Maybe Manager -> IO BSL.ByteString
fetchRawFromEnv mMgr =
  dataSourceFromEnv >>= \case
    DsStock -> loadDataFile "data/api_start2.json.xz"
    DsGitHub {dsgUser, dsgRepo, dsgBranch, dsgPath} -> do
      mgr <- ensureManager
      sha <- do
        let url =
              intercalate
                "/"
                [ "https://api.github.com/repos"
                , dsgUser
                , dsgRepo
                , "branches"
                , dsgBranch
                ]
        reqPre <- parseRequest url
        let req =
              reqPre
                { requestHeaders =
                    ("Accept", "application/vnd.github.v3+json") :
                    requestHeaders reqPre
                }
        resp <- httpLbs req mgr
        repoInfo <- case eitherDecode' @Value (responseBody resp) of
          Left msg -> die $ "error when resolving GitHub commit: " <> msg
          Right v -> pure v
        pure $ repoInfo |-- ["commit", "sha"]
      let url =
            intercalate
              "/"
              [ "https://raw.githubusercontent.com"
              , dsgUser
              , dsgRepo
              , T.unpack sha
              , dsgPath
              ]
      getResourceFromUrl mgr url
    DsUrl url -> do
      mgr <- ensureManager
      getResourceFromUrl mgr url
    DsFile fp ->
      toPlainData fp <$> BSL.readFile fp
  where
    ensureManager :: IO Manager
    ensureManager = case mMgr of
      Just m -> pure m
      Nothing -> newManager tlsManagerSettings
    getResourceFromUrl mgr url = do
      req <- parseRequest url
      let reqPath = T.unpack $ decodeUtf8 $ path req
      resp <- httpLbs req mgr
      pure (toPlainData reqPath $ responseBody resp)

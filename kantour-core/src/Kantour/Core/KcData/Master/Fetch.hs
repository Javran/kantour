{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Kantour.Core.KcData.Master.Fetch
  ( DataSource (..)
  , dataSourceP
  , dataSourceFromEnv
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

import Data.Aeson
import Data.List
import qualified Data.Text as T
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

data FileMetadata = FileMetadata
  { fmSource :: DataSource
  , fmCommit :: T.Text
  }

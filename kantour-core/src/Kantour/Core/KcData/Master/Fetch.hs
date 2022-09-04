module Kantour.Core.KcData.Master.Fetch (
  DataSource (..),
  dataSourceP,
  dataSourceFromEnv,
  FileMetadata,
  fmSource,
  fmCommit,
  toFileMetadata,
  cacheBaseFromEnv,
  fetchRawFromEnv,
  fetchRaw,
  parseRoot,
  fetch,
  fetchFromEnv,
) where

{-
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

 -}

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Aeson.Picker
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Yaml as Yaml
import qualified Dhall as D
import GHC.Generics (Generic)
import Kantour.Core.DataFiles
import Kantour.Core.KcData.Master.Direct.Root
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
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
  deriving (Eq, Generic)

instance D.FromDhall DataSource

instance Show DataSource where
  show = \case
    DsStock -> "stock"
    DsGitHub {dsgUser, dsgRepo, dsgBranch, dsgPath} ->
      intercalate ":" ["github", dsgUser, dsgRepo, dsgBranch, dsgPath]
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

  avoid direct construction but use `toFileMetadata` to enforce those invariants.

  Note: we can probably detect changes in HTTP [ETag](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/ETag)
    to figure out whether the data has been changed.
    Unfortunately githubusercontent does not seem to update this even when content is changed.

 -}
data FileMetadata = FileMetadata
  { fmSource :: DataSource
  , fmCommit :: Maybe T.Text
  }
  deriving (Eq, Show)

toFileMetadata :: MonadFail m => DataSource -> Maybe T.Text -> m FileMetadata
toFileMetadata fmSource fmCommit = do
  case fmSource of
    DsGitHub {} ->
      when (isNothing fmCommit) $
        fail "`commit` field required for `github:` source"
    DsUrl {} ->
      when (isJust fmCommit) $
        fail "`commit` field should not appear for `url:`"
    _ -> fail "only `github:` or `url:` is allowed as file metadata"
  pure $ FileMetadata {fmSource, fmCommit}

instance FromJSON FileMetadata where
  parseJSON = withObject "FileMetadata" $ \o ->
    join (toFileMetadata <$> o .: "source" <*> o .:? "commit")

instance ToJSON FileMetadata where
  toJSON FileMetadata {fmSource, fmCommit} = object ["source" .= fmSource, "commit" .= fmCommit]

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

cacheBaseToDataPath, cacheBaseToMetadataPath :: FilePath -> FilePath
cacheBaseToDataPath = (</> "api_start2.json")
cacheBaseToMetadataPath = (</> "api_start2.source.yaml")

loadFileMetadata :: FilePath -> IO (Maybe FileMetadata)
loadFileMetadata cb = runMaybeT $ do
  -- for the data file itself, we simply just require it to exist.
  True <-
    lift $
      doesFileExist (cacheBaseToDataPath cb)
  -- load metadata, validation of its format is done by calling the smart constructor in FromJSON instance.
  Right v <-
    lift $
      Yaml.decodeFileEither @FileMetadata (cacheBaseToMetadataPath cb)
  pure v

{-
  Fetches raw api_start2.json.

  - mMgr: optional Manager, this package will use its own if not provided.
  - src: DataSource to indicate where to download
  - mCacheBase: where should we read / write cache from / to.
    caching is disable when this is Nothing.

 -}
fetchRaw :: Maybe Manager -> DataSource -> Maybe FilePath -> IO BSL.ByteString
fetchRaw mMgr src mCacheBase = case src of
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
                  ("User-Agent", "github/Javran/kantour") :
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
    newMd <- toFileMetadata src (Just sha)
    getResourceFromUrl mgr url newMd
  DsUrl url -> do
    mgr <- ensureManager
    newMd <- toFileMetadata src Nothing
    getResourceFromUrl mgr url newMd
  DsFile fp ->
    toPlainData fp <$> BSL.readFile fp
  where
    ensureManager :: IO Manager
    ensureManager = case mMgr of
      Just m -> pure m
      Nothing -> newManager tlsManagerSettings
    getResourceFromUrl :: Manager -> String -> FileMetadata -> IO BSL.ByteString
    getResourceFromUrl mgr url newMd = do
      mCurMd <-
        maybe
          -- environment is not set, disable caching entirely.
          (pure Nothing)
          -- should use cache, but metadata could be malformed.
          loadFileMetadata
          mCacheBase
      if Just newMd == mCurMd
        then {-
               note: `isJust mCurMd` implies `isJust mCacheBase`,
               so `fromJust mCacheBase` is safe.
              -}
          BSL.readFile (cacheBaseToDataPath $ fromJust mCacheBase)
        else do
          req <- parseRequest url
          let reqPath = T.unpack $ decodeUtf8 $ path req
          resp <- httpLbs req mgr
          let rawContent = toPlainData reqPath $ responseBody resp
          maybe
            (pure ())
            ( \cb -> do
                BSL.writeFile (cacheBaseToDataPath cb) rawContent
                Yaml.encodeFile (cacheBaseToMetadataPath cb) newMd
            )
            mCacheBase
          pure rawContent

fetchRawFromEnv :: Maybe Manager -> IO BSL.ByteString
fetchRawFromEnv mMgr =
  join $
    fetchRaw mMgr
      <$> dataSourceFromEnv
      <*> cacheBaseFromEnv

parseRoot :: BSL.ByteString -> IO Root
parseRoot raw =
  case Data.Aeson.eitherDecode raw of
    Left msg -> die ("parse error: " <> msg)
    Right r -> pure r

fetch :: Maybe Manager -> DataSource -> Maybe FilePath -> IO Root
fetch mMgr src mCacheBase = fetchRaw mMgr src mCacheBase >>= parseRoot

fetchFromEnv :: Maybe Manager -> IO Root
fetchFromEnv mMgr = fetchRawFromEnv mMgr >>= parseRoot

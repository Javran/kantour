{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}
module Kantour.MapTool.MapConfig where

import Data.Aeson
import Data.Aeson.Types

data MapConfig = MapConfig
  { sourceFile :: FilePath
  , extraSources :: [MapExtraSource]
  } deriving Show

data MapExtraSource = MapExtraSource
  { sourceFile :: FilePath
  , matchString :: Maybe String
  } deriving Show

instance FromJSON MapConfig where
    parseJSON = withObject "MapConfig" $ \v -> MapConfig
        <$> v .: "source"
        <*> v .: "extras"

instance FromJSON MapExtraSource where
    parseJSON = withObject "MapExtraSource" $ \v -> MapExtraSource
        <$> v .: "source"
        <*> v .:? "match-string"

{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.Furnituregraph (
  Furnituregraph (..),
) where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Kantour.Core.KcData.Master.Direct.Common

data Furnituregraph = Furnituregraph
  { version :: T.Text
  , filename :: T.Text
  , fgType :: Int
  , fgId :: Int
  , no :: Int
  }
  deriving stock (Generic, Show)

instance FromJSON Furnituregraph where
  parseJSON = parseKcMstJson [("fgType", "type"), ("fgId", "id")]

instance NFData Furnituregraph

instance HasKnownFields Furnituregraph where
  knownFields _ =
    kcFields
      "version filename type id no"

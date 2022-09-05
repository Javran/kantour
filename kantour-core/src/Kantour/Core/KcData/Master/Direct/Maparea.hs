{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.Maparea (
  Maparea (..),
) where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Kantour.Core.KcData.Master.Direct.Common

data Maparea = Maparea
  { kcType :: Int
  , kcId :: Int
  , name :: T.Text
  }
  deriving stock (Generic, Show)

instance FromJSON Maparea where
  parseJSON = parseKcMstJson

instance NFData Maparea

instance HasKnownFields Maparea where
  knownFields _ = kcFields "type id name"

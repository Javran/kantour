{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.Bgm (
  Bgm (..),
) where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Kantour.Core.KcData.Master.Direct.Common

data Bgm = Bgm
  { kcId :: Int
  , name :: T.Text
  }
  deriving stock (Generic, Show)

instance FromJSON Bgm where
  parseJSON = parseKcMstJson

instance NFData Bgm

instance HasKnownFields Bgm where
  knownFields _ = kcFields "id name"

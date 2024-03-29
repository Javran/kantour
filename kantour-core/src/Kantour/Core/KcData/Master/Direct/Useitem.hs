{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.Useitem (
  Useitem (..),
) where

import Data.Aeson
import qualified Data.Text as T
import Kantour.Core.KcData.Master.Direct.Common

data Useitem = Useitem
  { usetype :: Int
  , category :: Int
  , price :: Int
  , kcId :: Int
  , name :: T.Text
  , description :: [T.Text]
  }
  deriving stock (Generic, Show)

instance FromJSON Useitem where
  parseJSON = parseKcMstJson

instance NFData Useitem
instance HasKnownFields Useitem where
  knownFields _ =
    kcFields
      "usetype category price id name description"

instance Verifiable Useitem

{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.Payitem (
  Payitem (..),
) where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Kantour.Core.KcData.Master.Direct.Common

data Payitem = Payitem
  { price :: Int
  , pType :: Int
  , pId :: Int
  , item :: [Int]
  , shopDescription :: T.Text
  , name :: T.Text
  , description :: T.Text
  }
  deriving stock (Generic, Show)

instance FromJSON Payitem where
  parseJSON = parseKcMstJson [("pType", "type"), ("pId", "id")]

instance NFData Payitem
instance HasKnownFields Payitem where
  knownFields _ =
    kcFields "price type id item shop_description name description"

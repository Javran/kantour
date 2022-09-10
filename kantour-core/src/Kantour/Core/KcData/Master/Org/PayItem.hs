{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.PayItem (
  PayItem (..),
) where

import qualified Data.Text as T
import qualified Kantour.Core.KcData.Master.Direct.Payitem as D
import Kantour.Core.KcData.Master.Org.Common

data PayItem = PayItem
  { price :: Int
  , kcType :: Int
  , kcId :: Int
  , items :: [Int]
  , shopDescription :: T.Text
  , name :: T.Text
  , description :: T.Text
  }
  deriving (Generic, Show)

instance NFData PayItem

instance FromDirect PayItem where
  type Source PayItem = D.Payitem

  fromDirect D.Payitem {price, kcType, kcId, item = items, shopDescription, name, description} =
    pure PayItem {price, kcType, kcId, items, shopDescription, name, description}

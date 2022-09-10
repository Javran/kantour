{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.UseItem (
  UseItem (..),
) where

import qualified Data.Text as T
import qualified Kantour.Core.KcData.Master.Direct.Useitem as D
import Kantour.Core.KcData.Master.Org.Common

data UseItem = UseItem
  { kcId :: Int
  , name :: T.Text
  , description :: [T.Text]
  , useType :: Int
  , category :: Int
  , price :: Int
  }
  deriving (Generic, Show)

instance NFData UseItem

instance FromDirect UseItem where
  type Source UseItem = D.Useitem

  fromDirect D.Useitem {kcId, name, description, usetype = useType, category, price} =
    pure UseItem {kcId, name, description, useType, category, price}

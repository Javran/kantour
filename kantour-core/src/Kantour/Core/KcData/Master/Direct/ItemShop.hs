module Kantour.Core.KcData.Master.Direct.ItemShop (
  ItemShop (..),
) where

import Data.Aeson as Aeson
import Kantour.Core.KcData.Master.Direct.Common

data ItemShop = ItemShop
  { cabinet_1 :: [Int]
  , cabinet_2 :: [Int]
  }
  deriving stock (Generic, Show)

instance FromJSON ItemShop where
  parseJSON = parseKcMstJson

instance NFData ItemShop

instance HasKnownFields ItemShop where
  knownFields _ = kcFields "cabinet_1 cabinet_2"

module Kantour.Core.KcData.Master.ItemShop (
  ItemShop (..),
) where

import Data.Aeson as Aeson
import Deriving.Aeson
import Kantour.Core.KcData.Master.Common

data ItemShop = ItemShop
  { cabinet_1 :: [Int]
  , cabinet_2 :: [Int]
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier KcConvention]
          ItemShop

instance HasKnownFields ItemShop where
  knownFields _ = kcFields "cabinet_1 cabinet_2"

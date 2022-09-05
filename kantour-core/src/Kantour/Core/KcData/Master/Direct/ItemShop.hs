module Kantour.Core.KcData.Master.Direct.ItemShop (
  ItemShop (..),
) where

import Data.Aeson as Aeson
import Deriving.Aeson
import Kantour.Core.KcData.Master.Direct.Common

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

instance NFData ItemShop

instance HasKnownFields ItemShop where
  knownFields _ = kcFields "cabinet_1 cabinet_2"

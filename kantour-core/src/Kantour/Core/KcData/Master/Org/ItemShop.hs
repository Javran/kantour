{-# OPTIONS_GHC -Wno-orphans #-}
module Kantour.Core.KcData.Master.Org.ItemShop (
  ItemShop (..),
) where

import Kantour.Core.KcData.Master.Direct.ItemShop
import Kantour.Core.KcData.Master.Org.Common

instance FromDirect ItemShop where
  type Source ItemShop = ItemShop

  fromDirect = pure

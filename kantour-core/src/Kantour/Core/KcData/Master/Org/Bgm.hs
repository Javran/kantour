{-# OPTIONS_GHC -Wno-orphans #-}
module Kantour.Core.KcData.Master.Org.Bgm (
  Bgm (..),
) where

import Kantour.Core.KcData.Master.Direct.Bgm
import Kantour.Core.KcData.Master.Org.Common

instance FromDirect Bgm where
  type Source Bgm = Bgm

  fromDirect = pure

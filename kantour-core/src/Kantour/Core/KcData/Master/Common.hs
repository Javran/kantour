{-# LANGUAGE DataKinds #-}
module Kantour.Core.KcData.Master.Common
  ( KcApiField
  , KcConvention
  )
where

import Deriving.Aeson

type KcConvention = [CamelToSnake, KcApiField]

data KcApiField

instance StringModifier KcApiField where
  getStringModifier = ("api_" <>)

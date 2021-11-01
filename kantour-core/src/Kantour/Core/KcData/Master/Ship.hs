{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Ship
  ( Ship (..)
  )
where

import Data.Aeson as Aeson
import Deriving.Aeson
import Kantour.Core.KcData.Master.Common

-- status: incomplete
data Ship = Ship
  { shipId :: Int
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier (Rename "shipId" "id" : KcConvention)]
          Ship

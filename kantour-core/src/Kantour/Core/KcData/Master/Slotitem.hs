{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Slotitem
  ( Slotitem (..)
  )
where

import Deriving.Aeson
import Kantour.Core.KcData.Master.Common

data Slotitem = Slotitem
  { slotId :: Int
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier (Rename "slotId" "id" : KcConvention)]
          Slotitem

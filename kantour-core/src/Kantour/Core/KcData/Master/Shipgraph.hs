{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Shipgraph
  ( Shipgraph (..)
  )
where

import Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Common

data Shipgraph = Shipgraph
  { version :: NE.NonEmpty T.Text
  , filename :: T.Text
  , shipId :: Int
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier (Rename "shipId" "id" : KcConvention)]
          Shipgraph

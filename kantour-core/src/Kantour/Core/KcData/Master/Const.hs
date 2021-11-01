{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Kantour.Core.KcData.Master.Const
  ( Const (..)
  )
where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Common

data IntValStrVal = IntValStrVal
  { intValue :: Int
  , stringValue :: T.Text
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier KcConvention]
          IntValStrVal

data Const = Const
  { bokoMaxShips :: IntValStrVal
  , parallelQuestMax :: IntValStrVal
  , dpflagQuest :: IntValStrVal
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier KcConvention]
          Const

instance HasKnownFields Const where
  knownFields _ =
    kcFields "boko_max_ships parallel_quest_max dpflag_quest"

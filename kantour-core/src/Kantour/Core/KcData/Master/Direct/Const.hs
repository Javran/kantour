module Kantour.Core.KcData.Master.Direct.Const (
  IntValStrVal (..),
  Const (..),
) where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Kantour.Core.KcData.Master.Direct.Common

data IntValStrVal = IntValStrVal
  { intValue :: Int
  , stringValue :: T.Text
  }
  deriving stock (Generic, Show)

instance FromJSON IntValStrVal where
  parseJSON = parseKcMstJson

instance NFData IntValStrVal

data Const = Const
  { bokoMaxShips :: IntValStrVal
  , parallelQuestMax :: IntValStrVal
  , dpflagQuest :: IntValStrVal
  }
  deriving stock (Generic, Show)

instance FromJSON Const where
  parseJSON = parseKcMstJson

instance NFData Const

instance HasKnownFields Const where
  knownFields _ =
    kcFields "boko_max_ships parallel_quest_max dpflag_quest"

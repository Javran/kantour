{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.Const (
  Const,
) where

import qualified Kantour.Core.KcData.Master.Direct.Const as D
import Kantour.Core.KcData.Master.Org.Common

data Const = Const
  { maxShips :: Int
  , maxParallelQuest :: Int
  , dpFlagQuest :: Int
  }
  deriving (Generic, Show)

instance NFData Const

instance FromDirect Const where
  type Source Const = D.Const

  fromDirect
    D.Const
      { bokoMaxShips
      , parallelQuestMax
      , dpflagQuest
      } = do
      let parse tag = \case
            D.IntValStrVal { stringValue = "", intValue = v} -> pure v
            _ -> illformed tag
      maxShips <- parse "bokoMaxShips" bokoMaxShips
      maxParallelQuest <- parse "parallelQuestMax" parallelQuestMax
      dpFlagQuest <- parse "dpflagQuest" dpflagQuest
      pure Const {maxShips , maxParallelQuest, dpFlagQuest}


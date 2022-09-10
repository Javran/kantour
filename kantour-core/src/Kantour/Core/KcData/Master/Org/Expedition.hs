{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.Expedition (
  Expedition (..),
) where

import qualified Data.Text as T
import qualified Kantour.Core.KcData.Master.Direct.Mission as D
import Kantour.Core.KcData.Master.Org.Common

data Expedition = Expedition
  { kcId :: Int
  , damageType :: Int
  , requiredShips :: Int
  , details :: T.Text
  , difficulty :: Int
  , dispNo :: T.Text
  , area :: Int
  , name :: T.Text
  , resetType :: Int
  , returnFlag :: Bool
  , sampleFleet :: [Int]
  , time :: Int
  , fuelAmmoCost :: (Double, Double)
  , winItems :: ((Int, Int), (Int, Int))
  , resourceGainRate :: (Int, Int, Int, Int)
  }
  deriving (Generic, Show)

instance NFData Expedition

instance FromDirect Expedition where
  type Source Expedition = D.Mission

  fromDirect
    D.Mission
      { kcId
      , damageType
      , deckNum = requiredShips
      , details
      , difficulty
      , dispNo
      , mapareaId = area
      , name
      , resetType
      , returnFlag = rf
      , sampleFleet = sf
      , time
      , useBull
      , useFuel
      , winItem1
      , winItem2
      , winMatLevel
      } = do
      returnFlag <- intBoolFlag "returnFlag" rf
      w1 <- case winItem1 of
        [a, b] -> pure (a, b)
        _ -> illformed "winItem1"
      w2 <- case winItem2 of
        [a, b] -> pure (a, b)
        _ -> illformed "winItem2"
      resourceGainRate <- case winMatLevel of
        [a, b, c, d] -> pure (a, b, c, d)
        _ -> illformed "winMatLevel"
      pure
        Expedition
          { kcId
          , damageType
          , requiredShips
          , details
          , difficulty
          , dispNo
          , area
          , name
          , resetType
          , returnFlag
          , sampleFleet = filter (/= 0) sf
          , time
          , fuelAmmoCost = (useFuel, useBull)
          , winItems = (w1, w2)
          , resourceGainRate
          }

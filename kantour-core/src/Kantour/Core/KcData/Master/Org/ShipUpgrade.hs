{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.ShipUpgrade (
  ShipUpgrade (..),
  canConvert,
) where

import Data.Maybe
import qualified Kantour.Core.KcData.Master.Direct.Shipupgrade as D
import Kantour.Core.KcData.Master.Org.Common

data ShipUpgrade = ShipUpgrade
  { shipIdFromTo :: (Int, Int)
  , level :: Int
  , shipIdOriginal :: Int
  , sortNo :: Int
  , aviationMat :: Int
  , blueprint :: Int
  , armsMat :: Int
  , report :: Int
  , catapult :: Int
  , boiler :: Int
  }
  deriving (Generic, Show)

instance NFData ShipUpgrade

canConvert :: D.Shipupgrade -> Bool
canConvert D.Shipupgrade {currentShipId = i} = i /= 0

instance FromDirect ShipUpgrade where
  type Source ShipUpgrade = D.Shipupgrade

  fromDirect
    D.Shipupgrade
      { kcId
      , currentShipId
      , upgradeLevel = level
      , originalShipId = shipIdOriginal
      , sortno = sortNo
      , aviationMatCount = aviationMat
      , drawingCount = blueprint
      , armsMatCount = armsMat
      , reportCount = report
      , catapultCount = catapult
      , boilerCount = b
      } = do
      when (currentShipId == 0) do
        throwError "currentShipId should not be 0"
      pure
        ShipUpgrade
          { shipIdFromTo = (currentShipId, kcId)
          , level
          , shipIdOriginal
          , sortNo
          , aviationMat
          , blueprint
          , armsMat
          , report
          , catapult
          , boiler = fromMaybe 0 b
          }

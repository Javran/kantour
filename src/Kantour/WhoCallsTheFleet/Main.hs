{-# LANGUAGE OverloadedStrings, TypeFamilies, DuplicateRecordFields #-}
module Kantour.WhoCallsTheFleet.Main where

import Data.Aeson

import Data.String
import Kantour.WhoCallsTheFleet.Types

import Kantour.WhoCallsTheFleet.Fetch (fetchShips)

parseShip :: String -> Maybe Ship
parseShip xs = decode (fromString xs)

defaultMain :: IO ()
defaultMain = do
    ships <- fetchShips
    mapM_ print ships

data Equipment = Equipment
  { masterId :: Int
  , eqpRarity :: Int
  , eqpType :: Int
  , eqpName :: () -- TODO
  , eqpStat :: ()
  , eqpDismantle :: ()
  , eqpCraftable :: Bool
  , eqpImprovable :: Bool
  , eqpRankUpgradable :: Bool
  }


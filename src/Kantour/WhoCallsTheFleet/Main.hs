{-# LANGUAGE OverloadedStrings, TypeFamilies, DuplicateRecordFields #-}
module Kantour.WhoCallsTheFleet.Main where

import Data.Aeson

import Data.String
import Kantour.WhoCallsTheFleet.Types
import Kantour.WhoCallsTheFleet.Fetch

parseShip :: String -> Maybe Ship
parseShip xs = decode (fromString xs)

defaultMain :: IO ()
defaultMain = do
    es <- fetchEquipments
    pure ()

{-# LANGUAGE OverloadedStrings, TypeFamilies, DuplicateRecordFields #-}
module Kantour.WhoCallsTheFleet.Main where

import Kantour.WhoCallsTheFleet.Fetch

defaultMain :: IO ()
defaultMain = do
    es <- fetchEquipments
    pure ()

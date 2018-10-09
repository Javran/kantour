{-# LANGUAGE OverloadedStrings, TypeFamilies, DuplicateRecordFields #-}
module Kantour.WhoCallsTheFleet.Main where

import Kantour.WhoCallsTheFleet.Fetch
import Kantour.Subcommand

data SubCmdWctf

instance Subcommand SubCmdWctf where
    name _ = "Wctf"
    main _ = defaultMain

defaultMain :: IO ()
defaultMain = do
    _ <- fetchEquipments
    pure ()

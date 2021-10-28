{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Kantour.WhoCallsTheFleet.Main where

import Kantour.Subcommand
import Kantour.WhoCallsTheFleet.Fetch

data SubCmdWctf

instance Subcommand SubCmdWctf where
  name _ = "Wctf"
  main _ = defaultMain

defaultMain :: IO ()
defaultMain = do
  _ <- fetchEquipments
  pure ()

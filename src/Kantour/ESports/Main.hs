module Kantour.ESports.Main where

import Kantour.Subcommand

data SubCmdESports

instance Subcommand SubCmdESports where
    name _ = "ESports"
    main _ = defaultMain

defaultMain :: IO ()
defaultMain = pure ()

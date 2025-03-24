{-# LANGUAGE TemplateHaskell #-}

module Kantour.Main
  ( defaultMain
  ) where

import Control.Monad
import Data.CaseInsensitive
import Data.Proxy
import Kantour.AirstrikeLeveling ()
import Kantour.AswEquip ()
import Kantour.Coded ()
import Kantour.DecMapUrl ()
import Kantour.DropCalc ()
import Kantour.GameServerLab ()
import Kantour.MapRedraw ()
import Kantour.MapTool.Main ()
import Kantour.MapToolFast.Main ()
import Kantour.MapTwol.Main ()
import Kantour.MasterLab ()
import Kantour.ShipStat.Main ()
import Kantour.Sprite ()
import Kantour.Subcommand
import Kantour.TH
import Kantour.WhoCallsTheFleet.Main ()
import System.Environment
import System.Exit
import Text.Printf

data ESub = forall sub. Subcommand sub => ESub (Proxy sub)

cmds :: [(CI String, IO ())]
cmds =
  mkInd <$> $(genSubcommands)
  where
    mkInd (ESub m) = (mk $ name m, main m)

defaultMain :: IO ()
defaultMain = do
  args <- getArgs
  case args of
    [] -> helpAndQuit
    (sub : subArgs) ->
      case lookup (mk sub) cmds of
        Nothing -> do
          printf "subcommand \"%s\" is not found\n" sub
          helpAndQuit
        Just x -> withArgs subArgs x
  where
    helpAndQuit = do
      putStrLn "Available commands are: (case-insensitive)\n"
      forM_ cmds $ \(sub, _) -> printf "- %s\n" (original sub)
      exitFailure

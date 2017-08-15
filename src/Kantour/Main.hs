{-# LANGUAGE ExistentialQuantification, TemplateHaskell #-}
module Kantour.Main
  ( defaultMain
  ) where

import Kantour.Subcommand

import System.Environment
import System.Exit
import Text.Printf
import Control.Monad
import Data.Proxy
import Kantour.TH

import Data.CaseInsensitive

import Kantour.Coded ()
import Kantour.DecMapUrl ()
import Kantour.ShipStat ()
import Kantour.AswEquip ()

import Kantour.DropCalc ()
import Kantour.MiniJson.Main ()
import Kantour.MapTool.Main ()
import Kantour.QuotesFetch.Main ()
import Kantour.WhoCallsTheFleet.Main ()

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
      (sub:subArgs) -> do
          let mSubCmd = lookup (mk sub) cmds
          case mSubCmd of
            Nothing -> do
                printf "subcommand \"%s\" is not found\n" sub
                helpAndQuit
            Just x -> withArgs subArgs x
  where
    helpAndQuit = do
      putStrLn "Available commands are: (case-insensitive)\n"
      forM_ cmds $ \(sub,_) -> printf "- %s\n" (original sub)
      exitFailure

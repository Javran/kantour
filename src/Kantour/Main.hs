{-# LANGUAGE ExistentialQuantification #-}
module Kantour.Main
  ( defaultMain
  ) where

import Kantour.Subcommand

import Data.CaseInsensitive
import System.Environment
import System.Exit
import Text.Printf
import Control.Monad
import Data.Proxy

import Kantour.Coded.Main (SubCmdCoded)
import Kantour.DecMapUrl.Main (SubCmdDecMapUrl)
import Kantour.DropCalc.Main (SubCmdDropCalc)
import Kantour.MiniJson.Main (SubCmdMiniJson)
import Kantour.MapTool.Main (SubCmdMapTool)
import Kantour.ShipStat.Main (SubCmdShipStat)
import Kantour.ASWEquip.Main (SubCmdAswEquip)
import Kantour.QuotesFetch.Main (SubCmdQuotesFetch)
import Kantour.WhoCallsTheFleet.Main (SubCmdWctf)

data ESub = forall sub. Subcommand sub => ESub (Proxy sub)

cmds :: [(CI String, IO ())]
cmds =
    mkInd <$>
      [ ESub (Proxy :: Proxy SubCmdCoded)
      , ESub (Proxy :: Proxy SubCmdDecMapUrl)
      , ESub (Proxy :: Proxy SubCmdDropCalc)
      , ESub (Proxy :: Proxy SubCmdMiniJson)
      , ESub (Proxy :: Proxy SubCmdMapTool)
      , ESub (Proxy :: Proxy SubCmdShipStat)
      , ESub (Proxy :: Proxy SubCmdAswEquip)
      , ESub (Proxy :: Proxy SubCmdQuotesFetch)
      , ESub (Proxy :: Proxy SubCmdWctf)
      ]
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

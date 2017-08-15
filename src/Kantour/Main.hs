{-# LANGUAGE ExistentialQuantification #-}
module Kantour.Main
  ( defaultMain
  ) where

import Kantour.Subcommand
import Kantour.Coded.Main (SubCmdCoded(..))
import Data.CaseInsensitive
import System.Environment
import System.Exit
import Text.Printf
import Control.Monad

data ESub = forall sub. Subcommand sub => ESub sub

cmds :: [(CI String, IO ())]
cmds = mkInd <$> [ESub SubCmdCoded]
  where
    mkInd (ESub m) = (mk $ name m, main m)

{-

to be recovered:

- dropcalc
- shipstat
- aswequip
- coded
- minijson
- maptool
- quotesfetch
- wctf
- decmapurl

-}

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

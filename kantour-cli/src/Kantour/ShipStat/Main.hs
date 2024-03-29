module Kantour.ShipStat.Main () where

import Control.Monad
import Data.Char
import Data.Coerce
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Data.Semigroup
import Kantour.Subcommand
import System.Environment
import System.Exit
import Text.Printf

import Kantour.ShipStat.Core
import Kantour.ShipStat.ProcessDump

data SubCmdShipStat

instance Subcommand SubCmdShipStat where
  name _ = "ShipStat"
  main _ = defaultMain

{-

this module is for estimating several ship stats: ASW, LoS and Evasion,
all of them depends on the level and few other hidden stats and is computed the same way.

file format:
- empty lines, and lines with leading "#" are ignored
- data line consists of 2 numbers separated by space: first is the level and second stat

example of a sample file:

```
# Taiyou ASW stat estimation

31 42
32 42
33 43
34 43
37 43
```

-}

processRaw :: String -> IM.IntMap Int
processRaw =
  enforceLength
    . IM.fromList
    . mapMaybe getDataLine
    . lines
  where
    enforceLength x =
      if IM.size x >= 2 then x else error "require at least 2 lines of data"
    getDataLine :: String -> Maybe (Int, Int)
    getDataLine raw = do
      -- has non-empty content
      guard (any (not . isSpace) raw)
      -- not a comment line
      guard (head raw /= '#')
      case words raw of
        [a, b] -> Just (read a, read b)
        _ -> Nothing

type ProgArgs = [String]

estimateStat :: ProgArgs -> IO ()
estimateStat [fp] = do
  d <- processRaw <$> readFile fp
  let results = computeStatInfo d
      pprStatInfoLine (StatInfo b df) = printf "base: %d, lv.99: %d\n" b (b + df)
  case results of
    [] -> putStrLn "search failed"
    [x] -> do
      putStr "single solution: "
      pprStatInfoLine x
    xs -> do
      putStrLn "possible stats are: "
      mapM_ pprStatInfoLine xs
  pure ()
estimateStat _ = error "shipstat est <stat file>"

calcStat :: ProgArgs -> IO ()
calcStat as = case as of
  (baseRaw : maxRaw : as')
    | [(baseSt, "")] <- reads baseRaw
      , [(maxSt, "")] <- reads maxRaw ->
      case as' of
        [] -> calcStat' baseSt maxSt Nothing
        [levelRaw]
          | [(level, "")] <- reads levelRaw ->
            calcStat' baseSt maxSt (Just level)
        _ -> failedPattern
  _ -> failedPattern
  where
    calcStat' :: Int -> Int -> Maybe Int -> IO ()
    calcStat' baseSt maxSt mLevel = case mLevel of
      Nothing -> do
        let statTable = map (\lvl -> (getStat' lvl, lvl)) [1 .. 165]
            statMinLvls = IM.fromListWith (<>) (coerce statTable :: [(Int, Min Int)])
            statList = IM.toAscList statMinLvls
        mapM_ (\(st, lvl) -> pprStat (coerce lvl :: Int) st) statList
      Just level -> pprStat level (getStat' level)
      where
        getStat' = getStat baseSt maxSt
        pprStat level st = printf "Level: %d\tStat: %d\n" level st

    failedPattern = error "shipstat calc <base> <max> [target level]"

defaultMain :: IO ()
defaultMain = do
  as <- getArgs
  case as of
    "est" : as' -> estimateStat as'
    "calc" : as' -> calcStat as'
    "process-dump" : as' -> processDump as'
    _ -> do
      putStrLn "shipstat <subcmd> [args] ..."
      putStrLn "subcmd: est / calc / process-dump"
      exitFailure

module Kantour.ShipStat.Main where

import System.Environment
import qualified Data.IntMap as IM
import Data.Maybe
import Control.Monad
import Data.Char

processRaw :: String -> IM.IntMap Int
processRaw =
      IM.fromList
    . mapMaybe getDataLine
    . lines
  where
    getDataLine :: String -> Maybe (Int,Int)
    getDataLine raw = do
        -- has non-empty content
        guard (any (not . isSpace) raw)
        -- not a comment line
        guard (head raw /= '#')
        case words raw of
            [a,b] -> Just (read a, read b)
            _ -> Nothing
{-

this module is for estimating several ship stats: ASW, LoS and Evasion,
all of them depends on the level and few other hidden stats and is computed the same way.

-}

defaultMain :: IO ()
defaultMain = do
    as <- getArgs
    case as of
        [fp] -> do
            d <- processRaw <$> readFile fp
            print d
            pure ()
        _ ->
            putStrLn "shipstat <stat file>"

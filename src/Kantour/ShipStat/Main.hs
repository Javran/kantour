module Kantour.ShipStat.Main where

import System.Environment
import qualified Data.IntMap as IM
import Data.Maybe
import Control.Monad
import Data.Char

getStat :: Int -> Int -> Int -> Int
getStat baseSt maxSt level = baseSt + floor lvlBonus
  where
    lvlBonus :: Double
    lvlBonus = fromIntegral (maxSt - baseSt) * fromIntegral level / 99

processRaw :: String -> IM.IntMap Int
processRaw =
      enforceLength
    . IM.fromList
    . mapMaybe getDataLine
    . lines
  where
    enforceLength x =
        if IM.size x >= 2 then x else error "require at least 2 lines of data"
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
            error "shipstat <stat file>"

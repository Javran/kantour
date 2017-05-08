module Kantour.ShipStat.Main where

import System.Environment
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Maybe
import Control.Monad
import Data.Char

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

{-# ANN module "HLint: ignore Avoid lambda" #-}


getStat :: Int -> Int -> Int -> Int
getStat baseSt maxSt level = baseSt + floor lvlBonus
  where
    lvlBonus :: Double
    lvlBonus = fromIntegral (maxSt - baseSt) * fromIntegral level / 99

findRange :: IM.IntMap Int -> ((Int, Int), (Int, Int))
findRange m =
    ( (minimum lv1Stats, maximum lv1Stats)
    , (minimum lv99Stats, maximum lv99Stats)
    )
  where
    keys = IM.keysSet m
    minLvl = IS.findMin keys
    Just minStat = IM.lookup minLvl m
    maxLvl = IS.findMax keys
    Just maxStat = IM.lookup maxLvl m

    estimatedRanges :: [(Int,Int)]
    estimatedRanges = do
        minStat' <- [minStat-1,minStat+1]
        maxStat' <- [maxStat-1,maxStat+1]
        pure (estimate' (minLvl,minStat') (maxLvl,maxStat'))

    lv1Stats = map fst estimatedRanges
    lv99Stats = map snd estimatedRanges

genRange :: ((Int,Int),(Int,Int)) -> [(Int, Int)]
genRange ((lv1A,lv1B), (lv99A,lv99B)) =
    [ (lv1,lv99)
    | lv1 <- [lv1A ..lv1B]
    , lv99 <- [lv99A..lv99B]
    ]

-- gives a value on Lv. 1 and Lv. 99
-- TODO: not sure whether it's Lv. 1 or 0 considered base stat
estimate' :: (Int, Int) -> (Int, Int) -> (Int, Int)
estimate' (x1, y1) (x2, y2) = (floor (le 1), ceiling (le 99))
  where
    le = linearEstimate (x1, y1) (x2, y2)

linearEstimate :: (Int, Int) -> (Int, Int) -> Int -> Double
linearEstimate (x1, y1) (x2, y2) x = fI x * k + b
  where
    fI :: Integral i => i -> Double
    fI = fromIntegral
    k = fI (y2 - y1) / fI (x2 - x1)
    b = fI y1 - k * fI x1

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

checkTruth :: (Int, Int) -> [(Int,Int)] -> [(Int,Int)]
checkTruth (lvl,stat) = filter check
  where
    check :: (Int, Int) -> Bool
    check (baseStat, maxStat) = getStat baseStat maxStat lvl == stat

defaultMain :: IO ()
defaultMain = do
    as <- getArgs
    case as of
        [fp] -> do
            d <- processRaw <$> readFile fp
            let searchSpace = genRange (findRange d)
                results = foldl (\curSearchSpace truth -> checkTruth truth curSearchSpace) searchSpace (IM.toList d)
            -- print searchSpace
            print results
            pure ()
        _ ->
            error "shipstat <stat file>"

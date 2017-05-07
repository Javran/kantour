module Kantour.ShipStat.Main where

import System.Environment
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Maybe
import Control.Monad
import Data.Char
import Data.Foldable

getStat :: Int -> Int -> Int -> Int
getStat baseSt maxSt level = baseSt + floor lvlBonus
  where
    lvlBonus :: Double
    lvlBonus = fromIntegral (maxSt - baseSt) * fromIntegral level / 99

findRange :: IM.IntMap Int -> ((Int, Int), (Int, Int))
findRange m =
    ( (minimum lv1Stats,maximum lv1Stats)
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
        pure (estimate' (minLvl,maxLvl) (minStat',maxStat'))

    lv1Stats = map fst estimatedRanges
    lv99Stats = map snd estimatedRanges

genRange :: ((Int,Int),(Int,Int)) -> [(Int, Int)]
genRange ((lv1A,lv1B), (lv99A,lv99B)) =
    [ (lv1,lv99)
    | lv1 <- [lv1A ..lv1B]
    , lv99 <- [lv99A,lv99B]
    ]

-- gives a value on Lv. 1 and Lv. 99
estimate' :: (Int, Int) -> (Int, Int) -> (Int, Int)
estimate' (minX, maxX) (minY, maxY) = (floor (le 1), ceiling (le 99))
  where
    le = linearEstimate (minX, maxX) (minY, maxY)

linearEstimate :: (Int, Int) -> (Int, Int) -> Int -> Double
linearEstimate (minX, maxX) (minY, maxY) x = fI x * k + b
  where
    fI :: Integral i => i -> Double
    fI = fromIntegral
    k = fI (maxY - minY) / fI (maxX - minX)
    b = fI minY - k * fI minX

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
            print (findRange d)
            pure ()
        _ ->
            error "shipstat <stat file>"

module Kantour.ShipStat.Core
  ( StatInfo(..)
  , mkStatInfo
  , computeStat
  , getStat
  , getPossibleStatInfo
  , elimStatInfo
  , computeStatInfo
  ) where

import Data.Function
import Control.Monad
import qualified Data.IntMap.Strict as IM

-- | compute stat at certain level given base stat and max stat (at Lv. 99)
getStat :: Int -> Int -> Int -> Int
getStat baseSt maxSt = computeStat (mkStatInfo baseSt maxSt)

computeStat :: StatInfo -> Int -> Int
computeStat (StatInfo baseSt stDiff) level = baseSt + floor lvlBonus
  where
    lvlBonus :: Double
    lvlBonus = fromIntegral stDiff * fromIntegral level / 99

data StatInfo = StatInfo
  { _baseSt :: Int
  , _stDiff :: Int
  } deriving Show

mkStatInfo :: Int -> Int -> StatInfo
mkStatInfo baseSt maxSt = StatInfo baseSt (maxSt - baseSt)

{-

Some ship stats depend merely on ship level (without taking modernization into account),
and the formula is (as implemented by function `getStat`):

> curSt = baseSt + floor((maxSt - baseSt) * level / 99)

Now our task is: given some pairs of (level, curSt),
we need to determine (baseSt, maxSt).

To simplify this a bit, let stDiff = maxSt - baseSt,
so we internally just need to determine the pair (baseSt, stDiff).

Step 1: figure out stDiff.

now consider a pair (level_1, curSt_1):

> curSt_1 = baseSt + floor(stDiff * level_1 / 99)
> curSt_1 = baseSt + stDiff*level_1/99 - c_1 (where 0 <= c_1 < 1) .... (1)

similarly for another pair (level_2, curSt_2):

> curSt_2 = baseSt + stDiff*level_2/99 - c_2 (where 0 <= c_2 < 1) .... (2)

now to cancel baseSt, we just do (2) - (1):

> curSt_2 - curSt_1 = stDiff*(level_2 - level_1)/99 - (c_2 - c_1)
> c_2 - c_1 = stDiff*(level_2 - level_1)/99 - (curSt_2 - curSt_1)

note that since 0 <= c_1 < 1 and 0 <= c_2 < 1, we know -1 < c_2 - c_1 < 1,
therefore:

> -1 < stDiff*(level_2 - level_1)/99 - (curSt_2 - curSt_1) < 1

Now let make some assumptions:

- level_2 > level_1
- ship stat never goes down as level increases, therefore curSt_2 >= curSt_1

under these assumptions, we can work out that:

- stDiff > ((curSt_2 - curSt_1) - 1)*99/(level_2 - level_1)
- stDiff < ((curSt_2 - curSt_1) + 1)*99/(level_2 - level_1)

knowing that stDiff has to be an integer, to compensate for potential
precision lose, we expand the range a little bit by rounding:

- stDiff >= floor(   ((curSt_2 - curSt_1) - 1)*99/(level_2 - level_1) )
- stDiff <= ceiling( ((curSt_2 - curSt_1) + 1)*99/(level_2 - level_1) )

Now we can use an nondeterministic computation to select stDiff.

Step 2: determine (stDiff, baseSt)

now after stDiff is determined (nondeterminstically), back to original formula:

> curSt_1 = baseSt + floor(stDiff * level_1 / 99)
> curSt_2 = baseSt + floor(stDiff * level_2 / 99)

which means:

> baseSt = curSt_1 - floor(stDiff * level_1 / 99) = curSt_2 - floor(stDiff * level_2 / 99)

guarding on this condition should be enough to fix baseSt and eliminate inconsistent stDiff,
leaving a small search space to work with

-}

-- (see comment above) computes an initial search space for StatInfo
getPossibleStatInfo :: (Int, Int) -> (Int, Int) -> [] StatInfo
getPossibleStatInfo (level1, curSt1) (level2, curSt2)
      -- enforcing invariant
    | level2 > level1 && curSt2 >= curSt1 = do
        let fDiv :: Int -> Int -> Double
            fDiv = (/) `on` fromIntegral
            stDiffUpper, stDiffLower :: Double
            stDiffLower = (((curSt2 - curSt1) - 1) * 99) `fDiv` (level2 - level1)
            stDiffUpper = (((curSt2 - curSt1) + 1) * 99) `fDiv` (level2 - level1)
        stDiff <- [floor stDiffLower .. ceiling stDiffUpper]
        let baseSt1 = curSt1 - floor((stDiff * level1) `fDiv` 99)
            baseSt2 = curSt2 - floor((stDiff * level2) `fDiv` 99)
        guard $ baseSt1 == baseSt2
        pure (StatInfo baseSt1 stDiff)
    | otherwise = []

-- use evidence to eliminate inconsistent results from a StatInfo search space
elimStatInfo :: (Int, Int) -> [] StatInfo -> [] StatInfo
elimStatInfo (level, stat)
    | level == 1 = filter (\(StatInfo baseSt _) -> baseSt == stat)
    | level == 99 = filter (\(StatInfo baseSt stDiff) -> baseSt + stDiff == stat)
    | otherwise =
        filter (\si -> stat == computeStat si level)

-- pairs should be given as IntMap to ensure uniqueness on level
computeStatInfo :: IM.IntMap Int -> [] StatInfo
computeStatInfo pairs
      -- at least 2 pairs to figure out an initial search space
    | IM.size pairs >= 2 =
        -- remove min, max level value from pairs
        let Just ((levelMin, stMin), pairs') = IM.minViewWithKey pairs
            Just ((levelMax, stMax), pairs'') = IM.maxViewWithKey pairs'
            initSearchSpace = getPossibleStatInfo (levelMin, stMin) (levelMax, stMax)
        in foldl (flip elimStatInfo) initSearchSpace (IM.toList pairs'')
    | otherwise = []

module Kantour.ShipStat.Core
  ( getStat
  ) where

-- | compute stat at certain level given base stat and max stat (at Lv. 99)
getStat :: Int -> Int -> Int -> Int
getStat baseSt maxSt level = baseSt + floor lvlBonus
  where
    lvlBonus :: Double
    lvlBonus = fromIntegral (maxSt - baseSt) * fromIntegral level / 99

{-

Some ship stats depend merely on ship level (without taking modernization into account),
and the formula is (as implemented by function `getStat`):

> curSt = baseSt + floor((maxSt - baseSt) * level / 99)

Now our task is: given some pairs of (level, curSt),
we need to determine (baseSt, maxSt).

To simplify this a bit, let stDiff = maxSt - baseSt,
so we internally just need to determine the pair (baseSt, stDiff).

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

also we know that stDiff has to be an integer:

- stDiff >= ceiling( ((curSt_2 - curSt_1) - 1)*99/(level_2 - level_1) )
- stDiff <= floor(   ((curSt_2 - curSt_1) + 1)*99/(level_2 - level_1) )

-}

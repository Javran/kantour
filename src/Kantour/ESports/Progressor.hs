{-# LANGUAGE LambdaCase, ExplicitForAll, ScopedTypeVariables, FlexibleContexts #-}
module Kantour.ESports.Progressor where

import Data.Semigroup
import Data.Ratio
import Data.Coerce
import Kantour.ESports.Quest

timeShort :: Sum Double
timeShort = Sum 0.5 -- for very simple nodes like 1-1A, 1-5A

timeNorm :: Sum Double
timeNorm = 1 -- normal node, no battleship

timeNormBB :: Sum Double
timeNormBB = Sum 1.5 -- normal node, has battleship

timeBoss :: Sum Double
timeBoss = Sum 1.5

timeBossBB :: Sum Double
timeBossBB = Sum 1.7 -- has battleship, might enter night battle

timeRefresh :: Sum Double
timeRefresh = Sum 0.2 -- for compass spin & auto refresh

timeRes :: Sum Double
timeRes = Sum 0.15 -- resource / maelstrom node

-- a progressor makes progress on some quests
data Progressor = Progressor
  { name :: String
  , timeCost :: Sum Double
  , progress :: QuestCode -> Sum Double
  }

mergeProb :: [(Rational, Sum Double)] -> Sum Double
mergeProb xs
    | total /= 1 = error "prob. should sum up to 1"
    | otherwise =
        let mul :: (Rational, Sum Double) -> Sum Double
            mul (x,y) = (Sum . fromRational $ x) * y
        in foldMap mul xs
  where
    total = sum (map fst xs)

{-

TODO

2-2
2-3/oa
2-3/subs
2-4
2-5/norm
2-5/Bm1
2-5/Bm7

3-1
3-2
3-3/airstrike
3-3/ref
3-4
3-5/north
3-5/south

4-1/oa
4-2/surface
4-2/subs
4-3
4-4
4-5

5-1/sinksub
5-1/Bm4
5-2
5-3
5-4
5-5

6-1
6-2
6-3/norm
6-3/Bq4
6-4
6-5

 -}

allProgressor :: [Progressor]
allProgressor =
    [ -- 1-1 sparkling
      mkP "1-1/sp"
        (timeShort <> timeShort)
        (\case
            "Bd1" -> 2 :: Double
            "Bd2" -> 2
            "Bd3" -> 2
            "Bw1/1" -> 1
            "Bw1/2" -> 1
            "Bw1/3" -> 0.5
            "Bw1/4" -> 0.5
            _ -> 0
        )
    , -- 1-1 one node then retreat
      mkP "1-1/one"
        timeShort
        (\case
             "Bd1" -> 1 :: Double
             "Bd2" -> 1
             "Bd3" -> 2
             "Bw1/1" -> 1
             "Bw1/2" -> 1
             _ -> 0
        )
    , -- 1-1 one node then refresh
      mkP "1-1/ref"
        (timeShort <> mergeProb [(1%2, timeRefresh), (1%2, timeShort)])
        (\case
             "Bd1" -> 1 + 0.5 :: Double
             _ -> 0
        )
    , -- 1-2, 1CV2SS, opening airstrike
      mkP "1-2"
        (mergeProb [(1%2, timeRefresh), (1%2, timeShort <> timeShort)])
        (\case
             "Bd1" -> mergeProb [(1%2, 0), (1%2, 2)]
             "Bd2" -> mergeProb [(1%2, 0), (1%2, 2)]
             "Bd3" -> mergeProb [(1%2, 0), (1%2, 2)]
             "Bw1/1" -> 1
             "Bw1/2" ->
                 mergeProb
                   [ (1%2, 0)
                     -- first node: easily S-rank, second node: lower chance of S-rank
                   , (1%2, 1 + mergeProb [(3%10, 1), (7%10, 0)])
                   ]
             "Bw1/3" -> mergeProb [(1%2, 0), (1%2, 1)]
             "Bw1/4" -> mergeProb [(1%2, 0), (1%2, 1)]
             _ -> mempty
        )
      -- 1-3: no worth it, ignoring.
    , -- 1-4 CL+DD
      mkP "1-4/cldd"
        (mergeProb
          [ (1%3, timeRefresh)
            -- possibly one extra node before boss
          , (2%3, timeNorm + timeRes + mergeProb [(1%2, timeRes), (1%2, timeNorm)] + timeNorm)
          ])
        (
            let -- 2 CV on node E (1/2) + 2 CV on node F
                cvSunk = mergeProb [(1%2, 2), (1%2, 0)] + 2
            in \case
                "Bd1" -> mergeProb [(1%3, 0), (2%3, 1)]
                "Bd2" -> mergeProb [(1%3, 0), (2%3, 1)]
                "Bd3" ->
                    mergeProb
                      [ (1%3, 0)
                      , (2%3, 1 + mergeProb [(1%2, 0), (1%2, 1)] + 1)
                      ]
                "Bd4" -> cvSunk
                "Bw1/1" -> 1
                "Bw1/2" ->
                    mergeProb
                      [ (1%3, 0)
                      , (2%3, 1 + mergeProb [(1%2, 0), (1%2, 1)] + 1)
                      ]
                "Bw1/3" -> mergeProb [(1%3, 0), (2%3, 1)]
                "Bw1/4" -> mergeProb [(1%3, 0), (2%3, 1)]
                "Bw2" -> cvSunk
                "Bm3" -> mergeProb [(1%3, 0), (2%3, 1)]
                _ -> mempty
        )
    , -- 1-4 airstrike, no refresh on G
      mkP "1-4/airstrike"
        (mergeProb
          [ -- A or B -> C -> D or E -> F
            (2%3, timeNorm <> timeRes <> mergeProb [(1%2, timeRes),(1%2, timeNorm)] <> timeNorm)
            -- G -> H -> I -> F or refresh
          , (1%3, timeRes <> timeRes <> timeNorm <> mergeProb [(1%2, timeNorm),(1%2, timeRefresh)])
          ]
        )
        (let cvSunk =
                 mergeProb
                   [ (2%3, mergeProb [(1%2, 2), (1%2, 0)] + 2)
                     -- I -> F or refresh
                   , (1%3, 2 + mergeProb [(1%2, 2), (1%2, 0)])
                   ]
         in \case
             "Bd1" -> 1
             "Bd2" -> 1
             "Bd3" ->
                 mergeProb
                   [ (2%3, 1 + mergeProb [(1%2,1),(1%2,0)] + 1)
                   , (1%3, 1 + mergeProb [(1%2,1),(1%2,0)])
                   ]
             "Bd4" -> cvSunk
             "Bw1/1" -> 1
             "Bw1/2" ->
                 mergeProb
                   [ (2%3, 1 + mergeProb [(1%2,1),(1%2,0)] + 1)
                   , (1%3, 1 + mergeProb [(1%2,1),(1%2,0)])
                   ]
             "Bw1/3" ->
                 mergeProb
                   [ (2%3, 1)
                   , (1%3, mergeProb [(1%2,1),(1%2,0)])
                   ]
             "Bw1/4" ->
                 mergeProb
                   [ (2%3, 1)
                   , (1%3, mergeProb [(1%2,1),(1%2,0)])
                   ]
             "Bw2" -> cvSunk
             _ -> mempty
        )
    , -- 1-5
      mkP "1-5" (stimes (4 :: Int) timeNorm) $
        let subSunk = 1 + 3 + 4 + 4
        in \case
            "Bd1" -> 1 :: Double
            "Bd2" -> 1
            "Bd3" -> 4
            "Bd8" -> subSunk
            "Bw1/1" -> 1
            "Bw1/2" -> 1
            "Bw1/3" -> 1
            "Bw1/4" -> 1
            "Bw5" -> subSunk
            "Bw10" -> 1
            "Bm5" -> 1
            "EO1-5" -> 1
            _ -> 0
    , -- 1-6 the normal way
      -- A (res) -> E -> G (res) -> F (norm+night, so use boss) -> B -> N
      mkP "1-6/norm" (timeRes + timeNorm + timeRes + timeBoss + timeNorm + timeRes) $
        let -- node F: 2/8 compo contains only 1CV, with rest containing 2
            -- let's just say we can sink 60% of them
            cvSunk = Sum 0.6 * mergeProb [(2%8, 1), (6%8, 2)]
            -- node E: 1/6: 3SS, 2/6: 4SS, 3/6: 5SS
            ssSunk = mergeProb [(1%6, 3), (2%6, 4), (3%6, 5)]
        in \case
            "Bd1" -> 1
            "Bd2" -> 1
            "Bd3" -> 3
            "Bd4" -> cvSunk
            "Bd8" -> ssSunk
            "Bw1/1" -> 1
            "Bw1/2" -> 1 + Sum (0.2 + 0.4) -- F + B, no high chance of S-rank
            "Bw1/3" -> 0
            "Bw1/4" -> 0 -- perhaps does not count as win
            "Bw2" -> cvSunk
            "Bw5" -> ssSunk
            "EO1-6" -> 1
            _ -> 0
    , -- 1-6, in the way only Tanaka thinks it's fun
      -- C -> H (res) -> K -> M (res) -> L -> I -> D -> N (res)
      mkP "1-6/stupid"
        (timeNormBB + timeRes + timeNormBB + timeRes + timeNorm + timeNormBB + timeNorm + timeRes) $
        let ssSunkC = Sum 0.9 * mergeProb [(1%6, 3), (2%6, 4), (3%6, 5)]
            cvSunkD = Sum 0.6 * mergeProb [(3%6, 2), (3%6, 3)]
            ssSunkI = Sum 0.7 * mergeProb [(1%3, 4), (2%3, 5)]
            cvSunkL = Sum 0.5 * mergeProb [(1%6, 2), (5%6, 3)]
            ssSunk = ssSunkC + ssSunkI
            cvSunk = cvSunkL + cvSunkD
        in \case
            "Bd1" -> 1
            "Bd2" -> 1
            "Bd3" -> 5
            "Bd4" -> cvSunk
            "Bd8" -> ssSunk
            "Bw1/1" -> 1
            "Bw1/2" -> 2 + Sum 0.4
            "Bw1/3" -> 0
            "Bw1/4" -> 0
            "Bw2" -> cvSunk
            "Bw5" -> ssSunk
            "Bq3" -> 1
            "EO1-6" -> 1
            _ -> 0
    , mkP "2-1" (timeNorm + timeRes + timeNorm) $
        let cvSunkC = mergeProb [(2%3, 3), (1%3, 2)]
            cvSunkE = mergeProb [(2%3, 2), (1%3, 3)]
            cvSunkF = cvSunkC
            cvSunk = mergeProb
                       [ (1%2, cvSunkC)
                       , (1%2, mergeProb [(1%2, cvSunkE), (1%2, cvSunkF)])
                       ]
            bossNodeExpect = mergeProb [(1%2, 0), (1%2, mergeProb [(1%2, 1), (1%2,0)])]
        in \case
            "Bd1" -> 2
            "Bd2" -> 2
            "Bd3" -> 2
            "Bd4" -> cvSunk
            "Bd7" -> bossNodeExpect
            "Bw1/1" -> 1
            "Bw1/2" -> 2
            "Bw1/3" -> bossNodeExpect
            "Bw1/4" -> bossNodeExpect
            "Bw2" -> cvSunk
            _ -> 0
    ]
  where
    mkP :: forall n1 n2. (Coercible n1 (Sum Double), Coercible n2 (Sum Double))
           => String -> n1 -> (String -> n2) -> Progressor
    mkP n tc mkProgress =
        Progressor n (coerce tc) (coerce mkProgress)

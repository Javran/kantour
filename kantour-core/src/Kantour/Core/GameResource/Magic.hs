module Kantour.Core.GameResource.Magic (
  defaultServer,
  servers,
  magicCode,
) where

import Data.Char
import qualified Data.IntMap.Strict as IM
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU

defaultServer :: T.Text
defaultServer = "w01y.kancolle-server.com"

servers :: IM.IntMap T.Text
servers =
  IM.fromList
    [ (1, defaultServer)
    , (2, "w02k.kancolle-server.com")
    , (3, "w03s.kancolle-server.com")
    , (4, "w04m.kancolle-server.com")
    , (5, "w05o.kancolle-server.com")
    , (6, "w06t.kancolle-server.com")
    , (7, "w07l.kancolle-server.com")
    , (8, "w08r.kancolle-server.com")
    , (9, "w09s.kancolle-server.com")
    , (10, "w10b.kancolle-server.com")
    , (11, "w11t.kancolle-server.com")
    , (12, "w12p.kancolle-server.com")
    , (13, "w13b.kancolle-server.com")
    , (14, "w14h.kancolle-server.com")
    , (15, "w15p.kancolle-server.com")
    , (16, "w16s.kancolle-server.com")
    , (17, "w17k.kancolle-server.com")
    , (18, "w18i.kancolle-server.com")
    , (19, "w19s.kancolle-server.com")
    , (20, "w20h.kancolle-server.com")
    ]

{- ORMOLU_DISABLE -}
resourceMagic :: VU.Vector Int
resourceMagic = VU.fromList
  [ 6657, 5699, 3371, 8909, 7719, 6229, 5449, 8561, 2987, 5501
  , 3127, 9319, 4365, 9811, 9927, 2423, 3439, 1865, 5925, 4409
  , 5509, 1517, 9695, 9255, 5325, 3691, 5519, 6949, 5607, 9539
  , 4133, 7795, 5465, 2659, 6381, 6875, 4019, 9195, 5645, 2887
  , 1213, 1815, 8671, 3015, 3147, 2991, 7977, 7045, 1619, 7909
  , 4451, 6573, 4545, 8251, 5983, 2849, 7249, 7449, 9477, 5963
  , 2711, 9019, 7375, 2201, 5631, 4893, 7653, 3719, 8819, 5839
  , 1853, 9843, 9119, 7023, 5681, 2345, 9873, 6349, 9315, 3795
  , 9737, 4633, 4173, 7549, 7171, 6147, 4723, 5039, 2723, 7815
  , 6201, 5999, 5339, 4431, 2911, 4435, 3611, 4423, 9517, 3243
  ]
{- ORMOLU_ENABLE -}

magicCode :: Int -> String -> Int
magicCode r seed =
  (17 * (r + 7) * (resourceMagic VU.! ((s + r * a) `rem` 100))) `rem` 8973 + 1000
  where
    s = getSum $ foldMap (Sum . ord) seed
    a = case seed of
      [] -> 1
      _ -> length seed

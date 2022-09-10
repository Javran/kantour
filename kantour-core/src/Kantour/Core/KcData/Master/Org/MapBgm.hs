{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.MapBgm (
  MapBgm (..),
) where

import qualified Kantour.Core.KcData.Master.Direct.Mapbgm as D
import Kantour.Core.KcData.Master.Org.Common

data MapBgm = MapBgm
  { kcId :: Int
  , areaNo :: (Int, Int)
  , movingBgm :: Int
  , mapBgm :: (Int, Int)
  , bossBgm :: (Int, Int)
  }
  deriving (Generic, Show)

instance NFData MapBgm

instance FromDirect MapBgm where
  type Source MapBgm = D.Mapbgm

  fromDirect
    D.Mapbgm
      { kcId
      , mapareaId
      , no
      , movingBgm
      , mapBgm = m
      , bossBgm = b
      } = do
      let tup2 tag = \case
            [x, y] -> pure (x, y)
            _ -> illformed tag
      mapBgm <- tup2 "mapBgm" m
      bossBgm <- tup2 "bossBgm" b
      pure
        MapBgm
          { kcId
          , areaNo = (mapareaId, no)
          , movingBgm
          , mapBgm
          , bossBgm
          }

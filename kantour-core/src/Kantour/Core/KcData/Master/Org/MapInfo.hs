{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.MapInfo (
  MapInfo (..),
) where

import qualified Data.Text as T
import qualified Kantour.Core.KcData.Master.Direct.Mapinfo as D
import Kantour.Core.KcData.Master.Org.Common

data MapInfo = MapInfo
  { kcId :: Int
  , areaNo :: (Int, Int)
  , level :: Int
  , name :: T.Text
  , operation :: T.Text
  , info :: T.Text
  , requiredDefeatCount :: Maybe Int
  , maxHp :: Maybe Int
  , sallyFlag :: (Int, Int, Int)
  , items :: (Int, Int, Int, Int)
  }
  deriving (Generic, Show)

instance NFData MapInfo

instance FromDirect MapInfo where
  type Source MapInfo = D.Mapinfo

  fromDirect
    D.Mapinfo
      { kcId
      , mapareaId
      , no
      , level
      , name
      , opetext = operation
      , infotext = info
      , requiredDefeatCount
      , maxMaphp = maxHp
      , sallyFlag = sf
      , item = ms
      } = do
      sallyFlag <- case sf of
        [a, b, c] -> pure (a, b, c)
        _ -> illformed "sallyFlag"
      items <- case ms of
        [a, b, c, d] -> pure (a, b, c, d)
        _ -> illformed "item"
      pure
        MapInfo
          { kcId
          , areaNo = (mapareaId, no)
          , level
          , name
          , operation
          , info
          , requiredDefeatCount
          , maxHp
          , sallyFlag
          , items
          }

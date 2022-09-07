{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.Equip (
  Equip (..),
) where

import qualified Data.Text as T
import GHC.Generics
import qualified Kantour.Core.KcData.Master.Direct.Slotitem as D
import Kantour.Core.KcData.Master.Org.Common

data Equip = Equip
  { mstId :: Int
  , name :: T.Text
  , version :: Maybe Int
  , antiAir :: Int -- tyku / 対空
  , range :: Int -- leng / 射程
  , firepower :: Int -- houg / 火力
  , antiSub :: Int -- tais / 対潜
  , los :: Int -- saku / 索敵
  , sortNo :: Int
  , kcType :: (Int, Int, Int, Int, Int)
  , torpedo :: Int -- raig / 雷装
  , bombing :: Int -- baku / 爆装
  , armor :: Int -- souk / 装甲
  , scrap :: (Int, Int, Int, Int) -- broken / 廃棄資材
  , sakb :: Int
  , raim :: Int
  , rare :: Int
  , accuracy :: Int -- houm / 命中 または 対爆(局地戦闘機の場合)
  , evasion :: Int -- houk / 回避 または 迎撃(局地戦闘機の場合)
  , costDist :: Maybe (Int, Int)
  , speed :: Int -- soku / 速力
  }
  deriving (Generic, Show)

instance NFData Equip

instance FromDirect Equip where
  type Source Equip = D.Slotitem

  fromDirect
    D.Slotitem
      { kcId = mstId
      , sakb
      , tyku = antiAir
      , leng = range
      , houg = firepower
      , version
      , tais = antiSub
      , houm = accuracy
      , saku = los
      , sortno = sortNo
      , kcType = kcTypePre
      , raig = torpedo
      , baku = bombing
      , souk = armor
      , broken
      , raim
      , name
      , rare
      , houk = evasion
      , cost
      , distance
      , soku = speed
      } = do
      kcType <- case kcTypePre of
        [a, b, c, d, e] -> pure (a, b, c, d, e)
        _ -> illformed "kcType"
      scrap <- case broken of
        [a, b, c, d] -> pure (a, b, c, d)
        _ -> illformed "broken"
      costDist <- case (cost, distance) of
        (Just c, Just d) -> pure $ Just (c, d)
        (Nothing, Nothing) -> pure Nothing
        _ -> illformed "cost, distance"
      pure
        Equip
          { mstId
          , sakb
          , antiAir
          , range
          , firepower
          , version
          , antiSub
          , accuracy
          , los
          , sortNo
          , kcType
          , torpedo
          , bombing
          , armor
          , scrap
          , raim
          , name
          , rare
          , evasion
          , costDist
          , speed
          }

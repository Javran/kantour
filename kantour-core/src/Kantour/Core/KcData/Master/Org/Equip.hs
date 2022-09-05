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
  , sakb :: Int
  , bakk :: Int
  , antiAir :: Int -- tyku / 対空
  , range :: Int -- leng / 射程
  , firepower :: Int -- houg / 火力
  , version :: Maybe Int
  , antiSub :: Int -- tais / 対潜
  , houm :: Int -- type-dependent
  , los :: Int -- saku / 索敵
  , luck :: Int
  , raik :: Int
  , sortNo :: Int
  , eType :: [Int]
  , torpedo :: Int -- raig / 雷装
  , bombing :: Int -- baku / 爆装
  , armor :: Int -- souk / 装甲
  , scrap :: (Int, Int, Int, Int) -- broken / 廃棄資材
  , taik :: Int
  , raim :: Int
  , name :: T.Text
  , usebull :: T.Text
  , atap :: Int
  , rare :: Int -- type-dependent
  , houk :: Int
  , cost :: Maybe Int
  , distance :: Maybe Int
  , speed :: Int -- soku / 速力
  }
  deriving (Generic, Show)

instance NFData Equip

instance FromDirect Equip where
  type Source Equip = D.Slotitem

  fromDirect
    D.Slotitem
      { slotId = eId
      , sakb
      , bakk
      , tyku = antiAir
      , leng = range
      , houg = firepower
      , version
      , tais = antiSub
      , houm
      , saku = los
      , luck
      , raik
      , sortno = sortNo
      , sType = eType
      , raig = torpedo
      , baku = bombing
      , souk = armor
      , broken
      , taik
      , raim
      , name
      , usebull
      , atap
      , rare
      , houk
      , cost
      , distance
      , soku = speed
      } = do
      scrap <- case broken of
        [a, b, c, d] -> pure (a, b, c, d)
        _ -> illformed "broken"
      pure
        Equip
          { eId
          , sakb
          , bakk
          , antiAir
          , range
          , firepower
          , version
          , antiSub
          , houm
          , los
          , luck
          , raik
          , sortNo
          , eType
          , torpedo
          , bombing
          , armor
          , scrap
          , taik
          , raim
          , name
          , usebull
          , atap
          , rare
          , houk
          , cost
          , distance
          , speed
          }

{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.Ship (
  After (..),
  SOurs (..),
  Ship (..),
) where

import qualified Data.Text as T
import qualified Kantour.Core.KcData.Master.Direct.Ship as D
import Kantour.Core.KcData.Master.Org.Common

data After = After
  { level :: Int
  , kcId :: Int
  , steelAmmo :: (Int, Int)
  }

type Stat = (Int, Int)

data SOurs = SOurs
  { sortNo :: Int
  , fuelAmmoMax :: (Int, Int)
  , hp :: Stat -- taik / 耐久
  , armor :: Stat -- souk / 装甲
  , firepower :: Stat -- houg / 火力
  , torpedo :: Stat -- raig / 雷装
  , antiAir :: Stat -- tyku / 対空
  , luck :: Stat
  , range :: Int -- leng / 射程
  , slotCapacity :: [Int]
  , buildTime :: Int
  , scrap :: (Int, Int, Int, Int)
  , powerUp :: (Int, Int, Int, Int)
  , rarity :: Int
  , intro :: T.Text
  , voiceFlag :: (Bool {-1-}, Bool {-2-}, Bool {-4-})
  , after :: Maybe After
  }

data Ship = Ship
  { kcId :: Int
  , sortId :: Int
  , name :: T.Text
  , yomi :: T.Text -- 読み
  , stype :: Int
  , ctype :: Int
  , tais :: Int
  , soku :: Int
  , slotNum :: Int
  , ours :: Maybe SOurs
  }

instance FromDirect Ship where
  type Source Ship = D.Ship

  fromDirect
    D.Ship
      { kcId
      , sortId
      , name
      , yomi
      , stype
      , ctype
      } = do
      error "TODO"
      pure
        Ship
          { kcId
          , sortId
          , name
          , yomi
          , stype
          , ctype
          }

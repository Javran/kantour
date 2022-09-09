{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.Ship (
  After (..),
  SOurs (..),
  Ship (..),
) where

import Data.Bits
import qualified Data.Text as T
import qualified Data.Text.Read as T
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
  , intro :: Maybe T.Text
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
  , antiSubBase :: Int -- tais / 対潜 (base)
  , speed :: Int -- soku / 速力
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
      , tais
      , soku = speed
      , slotNum
      , sortno
      , bullMax
      , fuelMax
      , taik
      , souk
      , houg
      , raig
      , tyku
      , luck = luckPre
      , leng
      , maxeq
      , buildtime
      , broken
      , powup
      , backs
      , getmes = intro
      , voicef
      , afterlv
      , aftershipid
      , afterfuel
      , afterbull
      } = do
      antiSubBase <- case tais of
        Just [t] -> pure t
        Nothing -> pure 0
        _ -> illformed "tais"
      let ours = do
            sortNo <- sortno
            fuelAmmoMax <- (,) <$> fuelMax <*> bullMax
            let stat v = do
                  [a, b] <- v
                  pure (a, b)
            hp <- stat taik
            armor <- stat souk
            firepower <- stat houg
            torpedo <- stat raig
            antiAir <- stat tyku
            luck <- stat luckPre
            range <- leng
            slotCapacity <- maxeq
            buildTime <- buildtime
            scrap <- do
              [a, b, c, d] <- broken
              pure (a, b, c, d)
            powerUp <- do
              [a, b, c, d] <- powup
              pure (a, b, c, d)
            rarity <- backs
            voiceFlag <- do
              val <- voicef
              pure (testBit val 0, testBit val 1, testBit val 2)
            pure
              SOurs
                { sortNo
                , fuelAmmoMax
                , hp
                , armor
                , firepower
                , torpedo
                , antiAir
                , luck
                , range
                , slotCapacity
                , buildTime
                , scrap
                , powerUp
                , rarity
                , intro
                , voiceFlag
                , after = do
                    level <- afterlv
                    kcIdAfter <- do
                      raw <- aftershipid
                      Right (v, "") <- pure $ T.decimal @Int raw
                      pure v
                    steelAmmo <- (,) <$> afterfuel <*> afterbull
                    pure After {level, kcId = kcIdAfter, steelAmmo}
                }
      pure
        Ship
          { kcId
          , sortId
          , name
          , yomi
          , stype
          , ctype
          , antiSubBase
          , speed
          , slotNum
          , ours
          }

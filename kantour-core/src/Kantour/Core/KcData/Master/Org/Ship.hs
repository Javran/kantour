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

-- TODO: some needs more intuitive names
data SOurs = SOurs
  { sortno :: Int
  , bullMax :: Int
  , fuelMax :: Int
  , taik :: [Int]
  , souk :: [Int]
  , houg :: [Int]
  , raig :: [Int]
  , tyku :: [Int]
  , luck :: [Int]
  , leng :: Int
  , maxeq :: [Int]
  , buildtime :: Int
  , broken :: [Int]
  , powup :: [Int]
  , backs :: Int
  , getmes :: T.Text
  , voiceFlag :: (Bool {-1-}, Bool {-2-}, Bool {-4-})
  , after :: Maybe After
  }

data Ship = Ship
  { kcId :: Int
  , sortId :: Int
  , name :: T.Text
  , yomi :: T.Text
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

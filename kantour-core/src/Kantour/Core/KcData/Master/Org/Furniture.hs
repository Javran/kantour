{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.Furniture (
  Furniture (..),
) where

import qualified Data.Text as T
import qualified Kantour.Core.KcData.Master.Direct.Furniture as D
import Kantour.Core.KcData.Master.Org.Common

data Furniture = Furniture
  { title :: T.Text
  , price :: Int
  , season :: Int
  , version :: Int
  , activeFlag :: Bool
  , outsideId :: Int
  , kcType :: Int
  , rarity :: Int
  , kcId :: Int
  , no :: Int
  , description :: T.Text
  , saleFlag :: Bool
  }
  deriving (Generic, Show)

instance NFData Furniture

instance FromDirect Furniture where
  type Source Furniture = D.Furniture

  fromDirect
    D.Furniture
      { title
      , price
      , season
      , version
      , activeFlag = afPre
      , outsideId
      , kcType
      , rarity
      , kcId
      , no
      , description
      , saleflg = sfPre
      } = do
      let toBool tag = \case
            0 -> pure False
            1 -> pure True
            _ -> illformed tag
      activeFlag <- toBool "activeFlag" afPre
      saleFlag <- toBool "saleflg" sfPre
      pure
        Furniture
          { title
          , price
          , season
          , version
          , activeFlag
          , outsideId
          , kcType
          , rarity
          , kcId
          , no
          , description
          , saleFlag
          }

{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.ShipType (
  ShipType (..),
) where

import qualified Data.IntMap as IM
import qualified Data.Text as T
import qualified Kantour.Core.KcData.Master.Direct.Stype as D
import Kantour.Core.KcData.Master.Org.Common
import qualified Data.HashMap.Strict as HM
import Control.Monad

data ShipType = ShipType
  { kcId :: Int
  , name :: T.Text
  , sortNo :: Int
  , kcnt :: Int
  , scnt :: Int
  , equipTypes :: IM.IntMap Bool
  }
  deriving (Generic, Show)

instance NFData ShipType

instance FromDirect ShipType where
  type Source ShipType = D.Stype

  fromDirect
    D.Stype
      { kcId
      , name
      , sortno = sortNo
      , kcnt
      , scnt
      , equipType = es
      } = do
      equipTypes <- IM.fromList <$> forM (HM.toList es) \(k,v) -> do
        k' <- case textToInt k of
                Just a -> pure a
                Nothing -> illformed $ "equipType.key" <> T.pack (show k)
        v' <- intBoolFlag "equipType.val" v
        pure (k', v')
      pure
        ShipType
          { kcId
          , name
          , sortNo
          , kcnt
          , scnt
          , equipTypes
          }

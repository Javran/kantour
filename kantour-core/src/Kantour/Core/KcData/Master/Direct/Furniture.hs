{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.Furniture (
  Furniture (..),
) where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Kantour.Core.KcData.Master.Direct.Common

data Furniture = Furniture
  { title :: T.Text
  , price :: Int
  , season :: Int
  , version :: Int
  , activeFlag :: Int
  , outsideId :: Int
  , kcType :: Int
  , rarity :: Int
  , kcId :: Int
  , no :: Int
  , description :: T.Text
  , saleflg :: Int
  }
  deriving stock (Generic, Show)

instance FromJSON Furniture where
  parseJSON = parseKcMstJson

instance NFData Furniture
instance HasKnownFields Furniture where
  knownFields _ =
    kcFields
      "title price season version active_flag outside_id \
      \type rarity id no description saleflg"

instance Verifiable Furniture where
  verify Furniture {kcId, kcType} = do
    unless (inRange (0, 5) kcType) do
      vLogS $
        "Furniture{" <> show kcId <> "}: type not in range [0..5]: " <> show kcType

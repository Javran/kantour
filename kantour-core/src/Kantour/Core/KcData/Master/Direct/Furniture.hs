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
  , season :: Maybe Int
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
  verify Furniture {kcId, kcType, activeFlag, rarity, saleflg} = do
    let warn msg = vLogS $ "Furniture{" <> show kcId <> "}: " <> msg
    unless (inRange (0, 5) kcType) do
      warn $ "type not in range [0..5]: " <> show kcType
    unless (inRange (0, 1) activeFlag) do
      warn $ "activeFlag not in range [0..1]: " <> show activeFlag
    unless (inRange (0, 6) rarity) do
      warn $ "rarity not in range [0..6]: " <> show rarity
    unless (inRange (0, 1) saleflg) do
      warn $ "saleflg not in range [0..1]: " <> show saleflg

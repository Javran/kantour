{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Kantour.WhoCallsTheFleet.Types.Equipment where

import GHC.Generics
import Data.Aeson

data Equipment = Equipment
  { masterId :: Int
  , rarity :: Int
  , eqpType :: Int
  , name :: () -- TODO
  , stat :: ()
  , dismantle :: ()
  , craftable :: Bool
  , improvable :: Bool
  , rankUpgradable :: ()
  } deriving (Generic, Show)

instance FromJSON Equipment where
    parseJSON = withObject "Equipment" $ \v -> Equipment
        <$> v .: "id"
        <*> v .: "rarity"
        <*> v .: "type"
        <*> pure ()
        <*> pure ()
        <*> pure ()
        <*> v .: "craftable"
        <*> v .: "improvable"
        <*> pure ()

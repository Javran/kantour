module Kantour.WhoCallsTheFleet.Types.Equipment where

data Equipment = Equipment
  { masterId :: Int
  , rarity :: Int
  , eqpType :: Int
  , name :: () -- TODO
  , stat :: ()
  , dismantle :: ()
  , craftable :: Bool
  , improvable :: Bool
  , rankUpgradable :: Bool
  }

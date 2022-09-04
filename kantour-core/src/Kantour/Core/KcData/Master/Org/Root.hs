module Kantour.Core.KcData.Master.Org.Root (
  Root (..),
) where

{-
  Org modules are organized version of the master data.

  TODO: WIP on those `:: ()` fields

 -}

data Root = Root
  { slotitem :: ()
  , shipgraph :: ()
  , ship :: ()
  , equipExslot :: ()
  , bgm :: ()
  , itemShop :: ()
  , const :: ()
  , equipExslotShip :: ()
  , equipShip :: ()
  , furniture :: ()
  , furnituregraph :: ()
  , maparea :: ()
  , mapbgm :: ()
  , mapinfo :: ()
  , mission :: ()
  , payitem :: ()
  , shipupgrade :: ()
  , slotitemEquiptype :: ()
  , stype :: ()
  , useitem :: ()
  }

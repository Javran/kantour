{-# LANGUAGE ScopedTypeVariables #-}
module Kantour.QuotesFetch.Quotes
  ( kc3Table
  , toKC3Key
  , kcwikiTable
  ) where

import qualified Data.IntMap as IM
import Data.Tuple
import Data.Maybe

kc3Table :: [(String, Int)]
kc3Table =
    [ ("Intro", 1), ("Library", 25)
    , ("Poke(1)", 2), ("Poke(2)", 3), ("Poke(3)", 4)
    , ("Married", 28), ("Wedding", 24), ("Ranking", 8), ("Join", 13)
    , ("Equip(1)", 9), ("Equip(2)", 10), ("Equip(3)", 26), ("Supply", 27)
    , ("Docking(1)", 11), ("Docking(2)", 12), ("Construction", 5)
    , ("Return", 7), ("Sortie", 14), ("Battle", 15), ("Attack", 16)
    , ("Yasen(1)", 18), ("Yasen(2)", 17), ("MVP", 23)
    , ("Damaged(1)", 19), ("Damaged(2)", 20), ("Damaged(3)", 21), ("Sunk", 22)
    , ("Idle", 29), ("Repair", 6)
    , ("H0000",30), ("H0100",31), ("H0200",32), ("H0300",33)
    , ("H0400",34), ("H0500",35), ("H0600",36), ("H0700",37)
    , ("H0800",38), ("H0900",39), ("H1000",40), ("H1100",41)
    , ("H1200",42), ("H1300",43), ("H1400",44), ("H1500",45)
    , ("H1600",46), ("H1700",47), ("H1800",48), ("H1900",49)
    , ("H2000",50), ("H2100",51), ("H2200",52), ("H2300",53)
    ]

toKC3Key :: Int -> String
toKC3Key sId = fromMaybe (show sId) (IM.lookup sId revTbl)
  where
    revTbl = IM.fromList (map swap kc3Table)

kcwikiTable :: [(String,Int)]
kcwikiTable =
    [ ("Intro", 1), ("LibIntro", 25)
    , ("Sec1", 2), ("Sec2", 3), ("Sec3", 4)
    , ("Return", 7), ("ConstComplete", 5), ("Achievement", 8)
    , ("Equip1", 9), ("Equip2", 10), ("Equip3", 26)
    , ("DockLightDmg", 11), ("DockMedDmg", 12), ("DockComplete", 6)
    , ("FleetOrg", 13)
    , ("Sortie", 14), ("Battle", 15)
    , ("Atk1", 16), ("Atk2", 17)
    , ("NightBattle", 18)
    , ("LightDmg1", 19), ("LightDmg2", 20), ("MedDmg", 21), ("Sunk", 22)
    , ("MVP", 23), ("Proposal", 24), ("Resupply", 27)
    , ("SecWed", 28), ("Idle", 29)
    , ("0000",30), ("0100",31), ("0200",32), ("0300",33)
    , ("0400",34), ("0500",35), ("0600",36), ("0700",37)
    , ("0800",38), ("0900",39), ("1000",40), ("1100",41)
    , ("1200",42), ("1300",43), ("1400",44), ("1500",45)
    , ("1600",46), ("1700",47), ("1800",48), ("1900",49)
    , ("2000",50), ("2100",51), ("2200",52), ("2300",53)
    ]

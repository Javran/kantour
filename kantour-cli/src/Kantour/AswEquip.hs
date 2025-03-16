{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Kantour.AswEquip where

import Data.List
import Data.Ord
import Kantour.Subcommand
import System.Environment
import Text.Printf

data SubCmdAswEquip

instance Subcommand SubCmdAswEquip where
  name _ = "AswEquip"
  main _ = defaultMain

{-

tool for finding the best ASW equipment combination

-}

data ASWType
  = DC
  | DCP
  | Sonar
  deriving (Eq, Show, Ord)

data ASWEquip = ASWE
  { desc :: String
  , aswStat :: Int
  , ty :: ASWType
  }
  deriving (Eq, Show, Ord)

aswEquips :: [ASWEquip]
aswEquips =
  [ ASWE "三式爆雷投射機" 8 DCP
  , ASWE "二式爆雷" 7 DC
  , ASWE "九五式爆雷" 4 DC
  , ASWE "三式水中探信儀" 10 Sonar
  , ASWE "四式水中聴音機" 12 Sonar
  ]

type EquipList = [ASWEquip]

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

firePower :: Int -> EquipList -> Double
firePower shipASW es =
  (2 * sqrt (fI shipASW) + 1.5 * aswSum + 13) * aswMod
  where
    aswSum = sum $ fI . aswStat <$> es
    aswMod = aswModifier es

aswModifier :: EquipList -> Double
aswModifier es = factorSonarDcp * factorExtra
  where
    -- existing one with sonar & DCP
    factorSonarDcp = if hasSonar && hasDcp then 1.15 else 1
    bonusSonarDc = if hasSonar && hasDc then 0.1 else 0
    bonusDcDcp = if hasDc && hasDcp then 0.15 else 0
    factorExtra = 1 + bonusSonarDc + bonusDcDcp

    tys = ty <$> es
    has t = t `elem` tys
    hasDcp = has DCP
    hasDc = has DC
    hasSonar = has Sonar

pickOne :: [a] -> _
pickOne [] = []
pickOne xs@(y : ys) =
  (y, xs) : -- with rep
  (y, ys) : -- w/o rep
  pickOne ys -- skip

genEquips :: Int -> [EquipList]
genEquips = compact . genEquips' aswEquips
  where
    genEquips' _ 0 = pure []
    genEquips' eqs n = do
      (e, eqs') <- pickOne eqs
      remained <- genEquips' eqs' (n - 1)
      pure (e : remained)

    compact = nub . (sort <$>)

printTable :: Int -> Int -> IO ()
printTable shipAsw slotCount = do
  printf "Ship ASW: %d, Slot Count: %d\n" shipAsw slotCount
  let rows = sortOn (Down . snd) $ (\es -> (es, firePower shipAsw es)) <$> genEquips slotCount
      ppr (es, fp) = printf "%s: %f\n" (intercalate " + " $ desc <$> es) fp
  mapM_ ppr rows

defaultMain :: IO ()
defaultMain = do
  args <- getArgs
  case args of
    [rawAsw] -> printTable (read rawAsw) 3
    [rawAsw, rawSlot] -> printTable (read rawAsw) (read rawSlot)
    _ -> error "aswequip <ship ASW stat> [<slot count>]"

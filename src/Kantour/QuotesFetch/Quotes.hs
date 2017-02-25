{-# LANGUAGE ScopedTypeVariables #-}
module Kantour.QuotesFetch.Quotes
  ( rqGetField
  , rqArchiveName, rqSituation
  , rqQuoteJP, rqQuoteSCN

  , kc3Table
  , kcwikiTable

  , Quote, mkQuote

  , processPage
  , renderAll
  ) where

import Control.Arrow
import Data.Tuple
import Text.PrettyPrint
import Data.List
import qualified Data.IntMap.Strict as IM
import Data.Either
import Kantour.Utils
import Data.Maybe
import Control.Monad
import Kantour.QuotesFetch.Types
import Kantour.QuotesFetch.ShipDatabase
import Data.Function

{-# ANN module "HLint: ignore Eta reduce" #-}

newtype Quote = Q (IM.IntMap String)

rqGetField :: String -> RawQuote -> Maybe String
rqGetField k rq = lookup k rq

rqArchiveName :: RawQuote -> Maybe String
rqArchiveName = rqGetField "档名"

rqSituation :: RawQuote -> Maybe String
rqSituation = rqGetField "场合"

rqQuoteJP :: RawQuote -> Maybe String
rqQuoteJP = rqGetField "日文台词"

rqQuoteSCN :: RawQuote -> Maybe String
rqQuoteSCN = rqGetField "中文译文"

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

-- assume a common prefix then convert raw quotes into a structured one.
-- all unrecognized pairs are outputed as well
mkQuote :: String -> [RawQuote] -> Maybe (Quote, [(String,String)])
mkQuote kcwikiId xs = do

    let -- (ls,rs) = partitionEithers $ map convertQuote xs
        convertQuote :: RawQuote -> Maybe (Either (String,String) (Int,String))
        convertQuote q = do
            k <- rqArchiveName q
            v <- rqQuoteSCN q
            case removePrefix (kcwikiId ++ "-") k of
                Nothing -> pure (Left (k,v))
                Just k' -> case lookup k' kcwikiTable of
                    Nothing -> pure (Left (k,v))
                    Just i -> pure (Right (i,v))
    results <- mapM convertQuote xs
    let (ls,rs) = partitionEithers results
    pure (Q . IM.fromList $ rs, ls)

mergeQuoteM :: Quote -> Quote -> IO Quote
mergeQuoteM (Q q1) (Q q2) = do
    let qi = q1 `IM.intersection` q2
    when (IM.size qi > 0) $
        putStrLn $ "Warning: overlapping quote keys: " ++ show (IM.keys qi)
    pure (Q (q1 `IM.union` q2))

processPage :: ShipDatabase -> (LinkName, RawPage) -> IO (IM.IntMap Quote)
processPage sdb (lName, (tRows, qSecs)) = do
    -- putStrLn $ "Processing link: " ++ lName
    let tabberNames = map fst tRows
        -- LId: library Id (used by kcwiki)
        getLId = takeWhile (/= '-')
        strWithCode s = s ++ " (" ++ show s ++ ")"
        processSection :: (SectionName, [RawQuote]) -> IO (Maybe (Int,Quote))
        processSection (secName, rqs) = do
            -- putStrLn $ "  Processing section: " ++ secName
            let lId = getLId . fromJust . rqArchiveName . head $ rqs
                -- codex id
                tbLId = lookup secName tRows
            unless (maybe True (== lId) tbLId) $
                putStrLn $
                  "Warning: inconsistent lib id: " ++ show (fromJust tbLId, lId)
            when (secName `notElem` (["报时","时报","语音存档"] ++ tabberNames)) $ do
                putStrLn $
                  "Warning: unrecognized section name: " ++ strWithCode secName
                putStrLn $ "Tabber: " ++ intercalate ", " (map strWithCode tabberNames)
            if secName /= "语音存档"
              then do
                let mstId = findMasterId lId sdb
                    mResult = mkQuote lId rqs
                case mResult of
                    Just (q,lft) -> do
                        unless (null lft) $ do
                            putStrLn $ "Warning: unconsumed quote input for link: "
                                ++ lName ++ " section: " ++ secName
                            let ppr (k,v) = putStrLn $ "W: (" ++ k ++ ", " ++ v ++ ")"
                            mapM_ ppr lft
                        pure (Just (mstId, q))
                    Nothing -> do
                       putStrLn $ "Warning: Failed to process quotes for link: "
                           ++ lName ++ " section: " ++ secName
                       pure Nothing
             else pure Nothing
    processed <- catMaybes <$> mapM processSection qSecs :: IO [(Int,Quote)]
    let merge :: IM.IntMap Quote -> (Int,Quote) -> IO (IM.IntMap Quote)
        merge acc (k,v) = maybe
          (pure (IM.insert k v acc))
          (\oldV -> do
               newV <- mergeQuoteM oldV v
               pure (IM.insert k newV acc))
          (IM.lookup k acc)
    foldM merge IM.empty processed :: IO (IM.IntMap Quote)

renderQuote :: [(String, Int)] -> Quote -> Doc
renderQuote tbl (Q q) = vcat (map (\(k,v) -> text (k ++ ": " ++ v)) xs)
  where
    revTbl = IM.fromList (map swap tbl)
    -- sorted for printing
    xs :: [(String,String)]
    xs = sortBy (compare `on` fst)
       . (map . first) (fromJust . (`IM.lookup` revTbl))
       . IM.toList
       $ q

renderAll :: ShipDatabase -> [(String, Int)] -> IM.IntMap Quote -> Doc
renderAll sdb tbl qs = vcat (map rdr xs)
  where
    rdr (mstId, q) = text (jp ++ " (" ++ show mstId ++ ")" ) $$ nest 2 (renderQuote tbl q)
      where
        (jp,_) = findShipName sdb mstId
    xs = IM.toAscList qs

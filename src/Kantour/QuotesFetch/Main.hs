{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Kantour.QuotesFetch.Main where

import Kantour.QuotesFetch.Fetch
import Kantour.QuotesFetch.ShipDatabase
import Kantour.QuotesFetch.Kcwiki
import Kantour.QuotesFetch.PageProcessor
import Kantour.QuotesFetch.ComponentParser
import Kantour.QuotesFetch.PageParser
import Kantour.QuotesFetch.Quotes

import Text.Megaparsec
import Data.Coerce
import Data.Maybe

import qualified Data.Text as T
import qualified Data.IntMap as IM
import qualified Data.Map.Strict as M
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Logger
import Control.Arrow
import Control.Concurrent.ParallelIO
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}

processAndCombine :: IO ShipQuoteTable
processAndCombine = do
    sdb <- shipDatabaseFromString True =<< fetchRawDatabase
    let links = map (\mstId -> snd {- both "fst" (jp) and "snd" (scn) should work fine -}
                               (findShipName sdb mstId))
              $ getOrigins sdb
        processLink link = do
            liftIO $ putStrLn $ "link: " ++ link
            content <- liftIO $ fetchWikiLink link
            case parse pScanAll "" content of
                Left err -> liftIO $ print err >> undefined
                Right (Page pgResult) -> do
                    let Just (trs,xs) = parseShipInfoPage (coerce pgResult)
                    processRegular (T.pack link) sdb (trs,xs)
    tqss <- parallel (map (runStdoutLoggingT . processLink) links)
    regulars1 <- runStdoutLoggingT $
        foldM (\acc i -> loggedSQTUnion "N/A" acc (IM.toList i)) IM.empty tqss
    let regulars = reapplyQuoteLines sdb regulars1
    content' <- fetchWikiLink "季节性/2017年女儿节"
    let Right (Page result') = parse pScanAll "" content'
        pageContent = fromJust (parseSeasonalPage result')
    seasonals <-
        reapplyQuoteLines sdb
        <$> runStdoutLoggingT (processSeasonal "main" sdb pageContent)
    runStdoutLoggingT (loggedSQTUnion "N/A" regulars (IM.toList seasonals))

defaultMain :: IO ()
defaultMain = do
    sqt <- processAndCombine
    stopGlobalPool
    let kc3qt = toKcwikiQuoteTable sqt
    LBS.writeFile "kcwiki.json" (encode kc3qt)

{-
type KC3QuoteTable = M.Map String (M.Map String String)

toKC3QuoteTable :: ShipQuoteTable -> KC3QuoteTable
toKC3QuoteTable = M.fromList . map f . IM.toList
  where
    f :: (Int, IM.IntMap QuoteLine) -> (String, M.Map String String)
    f = show *** convert
    convert :: IM.IntMap QuoteLine -> M.Map String String
    convert =
          M.fromList
        . mapMaybe convertPair
        . IM.toList
    convertPair :: (Int, QuoteLine) -> Maybe (String, String)
    convertPair (sId, ql) = do
        scn <- qlTextSCN ql
        pure (toKC3Key sId, scn) -}

toKcwikiQuoteTable :: ShipQuoteTable -> M.Map String (M.Map String String)
toKcwikiQuoteTable = M.fromList . map f . IM.toList
  where
    f :: (Int, IM.IntMap QuoteLine) -> (String, M.Map String String)
    f = show *** convert
    convert :: IM.IntMap QuoteLine -> M.Map String String
    convert =
          M.fromList
        . mapMaybe convertPair
        . IM.toList
    convertPair :: (Int, QuoteLine) -> Maybe (String, String)
    convertPair (sId, ql) = do
        scn <- qlTextSCN ql
        pure (show sId, scn)

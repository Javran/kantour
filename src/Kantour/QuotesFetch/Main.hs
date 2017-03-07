{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Kantour.QuotesFetch.Main where

import Kantour.QuotesFetch.Fetch
import Kantour.QuotesFetch.ShipDatabase
import Kantour.QuotesFetch.Kcwiki
import Kantour.QuotesFetch.PageProcessor
import Kantour.QuotesFetch.ComponentParser
import Kantour.QuotesFetch.PageParser

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
import GHC.Conc.Sync
import System.Environment
import Kantour.QuotesFetch.SimpleLogger
import Data.Typeable
import Control.Exception

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}

data QuoteFetchException =
    -- QFE <source> <message>
    QuoteFetchException
    { qfeSource :: String
    , qfeMessage :: String
    } deriving (Show, Typeable)

instance Exception QuoteFetchException

processAndCombine :: String -> IO ShipQuoteTable
processAndCombine seasonalLink = do
    sdb <- shipDatabaseFromString True =<< fetchRawDatabase
    let links = map (\mstId -> snd {- both "fst" (jp) and "snd" (scn) should work fine -}
                               (findShipName sdb mstId))
              $ getOrigins sdb
        processLink :: String -> IO (ShipQuoteTable, [LogMessage])
        processLink link = do
            content <- fetchWikiLink link
            let errSrc = "Link:" ++ link
            case parse pScanAll "" content of
                Left err ->
                    let msg = "PageParsing failed: " ++ show err
                    in throw (QuoteFetchException errSrc msg)
                Right (Page pgResult) -> case parseShipInfoPage (coerce pgResult) of
                    Just (trs,xs) -> pure $ runSimpleLogger $ do
                            xs' <- mapM (\(h,qls) ->
                                         removeEmptyQuoteLines qls
                                         >>= \qls' -> pure (h,qls')) xs
                            processRegular (T.pack link) sdb (trs,xs')
                    Nothing ->
                        throw (QuoteFetchException errSrc "ComponentParsing failed")
    cn <- getNumCapabilities
    putStrLn $ "# of capabilities: " ++ show cn
    -- TODO: explicit exception
    tqss1 <- parallelInterleaved (map processLink links)
    let tqss = fst <$> tqss1
        logs = concat $ snd <$> tqss1
    putStrLn "" >> putStrLn "Parallel fetch completed."
    putStrLn "=== Fetch log begin"
    mapM_ (putStrLn . logMessageToStr) logs
    putStrLn "=== Fetch log end"
    regulars1 <- runStdoutLoggingT $
        foldM (\acc i -> loggedSQTUnion "mergeRegulars" acc (IM.toList i)) IM.empty tqss
    let regulars = reapplyQuoteLines sdb regulars1
    content' <- fetchWikiLink seasonalLink
    let Right (Page result') = parse pScanAll "" content'
        pageContent = fromJust (parseSeasonalPage result')
    pageContent' <- runStdoutLoggingT (removeEmptyQuoteLines pageContent)
    seasonals <-
        reapplyQuoteLines sdb
        <$> runStdoutLoggingT (processSeasonal "mergeSeasonals" sdb pageContent')
    runStdoutLoggingT (loggedSQTUnion "finalCombine" regulars (IM.toList seasonals))

defaultMain :: IO ()
defaultMain = do
    --
    as <- getArgs
    seasonalLink <- case as of
        [s] -> pure s
        _ -> do
            let defLink = "季节性/2017年白色情人节"
            putStrLn "argument not recognized, falling back to use default link"
            pure defLink
    sqt <- processAndCombine seasonalLink
    stopGlobalPool
    let kc3qt = toKcwikiQuoteTable sqt
    LBS.writeFile "kcwiki.json" (encode kc3qt)

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

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, GADTs, DataKinds #-}
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
import Data.Either
import Control.Exception
import Kantour.Utils

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}
{-# ANN module ("HLint: ignore Use unless" :: String) #-}

data QuoteFetchException =
    -- QFE <source> <message>
    QuoteFetchException
    { qfeSource :: String
    , qfeMessage :: String
    } deriving (Show, Typeable)

instance Exception QuoteFetchException

processAndCombine :: Maybe String -> IO ShipQuoteTable
processAndCombine mSeasonalLink = do
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
                    Just (PgShipInfo trs xs) -> pure $ runSimpleLogger $ do
                            xs' <- mapM (\(h,qls) ->
                                         removeEmptyQuoteLines qls
                                         >>= \qls' -> pure (h,qls')) xs
                            processRegular (T.pack link) sdb (trs,xs')
                    Nothing ->
                        throw (QuoteFetchException errSrc "ComponentParsing failed")
    cn <- getNumCapabilities
    putStrLn $ "# of capabilities: " ++ show cn
    tqss1E <- parallelInterleavedE (map processLink links)
    let (tqssErrs,tqss1) = partitionEithers tqss1E
        tqss = fst <$> tqss1
        logs = concat $ snd <$> tqss1
    putStrLn "" >> putStrLn "Parallel fetch completed."
    putStrLn "=== Fetch log begin"
    mapM_ (putStrLn . logMessageToStr) logs
    putStrLn "=== Fetch log end"
    when (not (null tqssErrs)) $ do
        putStrLn "Some tasks failed:"
        mapM_ (\e -> case fromException e of
                   Just (QuoteFetchException src msg) -> do
                       putStrLn $ "  Src: " ++ src
                       putStrLn $ "  Msg: " ++ msg
                   Nothing -> print e) tqssErrs
    regulars1 <- runStdoutLoggingT $
        foldM (\acc i -> loggedSQTUnion "mergeRegulars" acc (IM.toList i)) IM.empty tqss
    let regulars = reapplyQuoteLines sdb regulars1
    seasonals <- case mSeasonalLink of
        Just seasonalLink -> do
            content' <- fetchWikiLink seasonalLink
            let Right (Page result') = parse pScanAll "" content'
                PgSeasonal pageContent = fromJust (parseSeasonalPage result')
            pageContent' <- runStdoutLoggingT (removeEmptyQuoteLines pageContent)
            reapplyQuoteLines sdb
                <$> runStdoutLoggingT (processPage "mergeSeasonals" sdb (PgSeasonal pageContent'))
        Nothing ->
            putStrLn "no seasonal link specified, using empty table instead."
            >> pure IM.empty
    runStdoutLoggingT (loggedSQTUnion "finalCombine" regulars (IM.toList seasonals))

defaultMain :: IO ()
defaultMain = do
    as <- getArgs
    let mSeasonalLink = exactlyOne as
    sqt <- processAndCombine mSeasonalLink
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

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
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Logger

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}

processAndCombine :: IO ()
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
    regulars <- runStdoutLoggingT $ do
        tqss <- mapM processLink links
        foldM (\acc i -> loggedSQTUnion "N/A" acc (IM.toList i)) IM.empty tqss

    content' <- fetchWikiLink "季节性/2017年节分季节"
    let Right (Page result') = parse pScanAll "" content'
        pageContent = fromJust (parseSeasonalPage result')
    seasonals <- runStdoutLoggingT (processSeasonal "main" sdb pageContent)
    fin <- runStdoutLoggingT (loggedSQTUnion "N/A" regulars (IM.toList seasonals))
    print (IM.size fin)

defaultMain :: IO ()
defaultMain = processAndCombine

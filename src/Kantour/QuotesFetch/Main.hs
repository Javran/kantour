{-# LANGUAGE ScopedTypeVariables #-}
module Kantour.QuotesFetch.Main where

import Kantour.QuotesFetch.Fetch
import Kantour.QuotesFetch.ShipDatabase
import Kantour.QuotesFetch.Quotes
import Kantour.QuotesFetch.Kcwiki
import Kantour.QuotesFetch.ComponentParser

import Text.PrettyPrint.HughesPJClass

import qualified Kantour.QuotesFetch.PageParser as PP
import Text.Megaparsec
import Data.Coerce
import Data.Maybe
import Kantour.QuotesFetch.PageProcessor
import Control.Monad.Logger
import qualified Data.Text as T

{-# ANN module "HLint: ignore Avoid lambda" #-}

processRegularQuotes :: IO ()
processRegularQuotes = do
    sdb <- getDatabase <$> fetchRawDatabase
    rqs <- fetchRawQuotes
    pages <- mapM (processPage sdb) rqs
    mapM_ (\page -> print (renderAll sdb kc3Table page)) pages

demoProcessRegularPage :: IO ()
demoProcessRegularPage = do
    content <- fetchWikiLink "天津风"
    case parse PP.pScanAll "" content of
        Left err -> print err
        Right (Page result) -> do
            let Just (trs,xs) = parseShipInfoPage (coerce result)
            putStrLn (prettyShow trs)
            let ppr (k,vs) = putStrLn (prettyShow k) >> putStrLn (prettyShow vs)
            mapM_ ppr xs

demoProcessSeasonalPage :: IO ()
demoProcessSeasonalPage = do
    sdb <- getDatabase <$> fetchRawDatabase
    content <- fetchWikiLink "季节性/2017年节分季节"
    let Right (Page result) = parse PP.pScanAll "" content
        pageContent = fromJust (parseSeasonalPage result)
    tbl <- runStdoutLoggingT (processSeasonal (T.pack "main") sdb pageContent)
    pure ()

dumpAllPages :: IO ()
dumpAllPages = do
    result <- fetchWikiLink "Template:舰娘导航"
    let (Right links) = parse PP.pCollectLinks "" result
    contents <- unlines <$> mapM fetchWikiLink links
    writeFile "packed.raw" contents

defaultMain :: IO ()
defaultMain = do
    demoProcessSeasonalPage

{-# LANGUAGE ScopedTypeVariables #-}
module Kantour.QuotesFetch.Main where

import Kantour.QuotesFetch.Fetch
import Kantour.QuotesFetch.ShipDatabase
import Kantour.QuotesFetch.Quotes
import Kantour.QuotesFetch.Kcwiki
import qualified Kantour.QuotesFetch.ProcessPage as PP

import Text.PrettyPrint.HughesPJClass

import qualified Kantour.QuotesFetch.QParser as QP
import Text.Megaparsec
import Control.Monad.State
import Data.Coerce
import Data.List

import System.IO
import Control.DeepSeq

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
    case parse QP.pScanAll "" content of
        Left err -> print err
        Right (Page result) -> do
            let Just (trs,xs) = evalState PP.processPage (coerce result)
            putStrLn (prettyShow trs)
            let ppr (k,vs) = putStrLn (prettyShow k) >> putStrLn (prettyShow vs)
            mapM_ ppr xs

demoProcessSeasonalPage :: IO ()
demoProcessSeasonalPage = do
    content <- fetchWikiLink "季节性/2017年节分季节"
    let Right (Page result) = parse QP.pScanAll "" content
        qs = qlArchive <$> PP.processPage2 result
    mapM_ print qs

dumpAllPages :: IO ()
dumpAllPages = do
    result <- fetchWikiLink "Template:舰娘导航"
    let (Right links) = parse QP.pCollectLinks "" result
    contents <- unlines <$> mapM fetchWikiLink links
    writeFile "packed.raw" contents

defaultMain :: IO ()
defaultMain = do
    content <- readFile "packed.raw"
    let Right (Page result) = parse QP.pScanAll "" content
    print (length (force result))

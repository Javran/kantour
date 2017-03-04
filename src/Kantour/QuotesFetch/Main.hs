{-# LANGUAGE ScopedTypeVariables #-}
module Kantour.QuotesFetch.Main where

import Kantour.QuotesFetch.Fetch
import Kantour.QuotesFetch.ShipDatabase
import Kantour.QuotesFetch.Quotes
import Kantour.QuotesFetch.Types
import Kantour.QuotesFetch.Kcwiki
import qualified Kantour.QuotesFetch.ProcessPage as PP

import Text.PrettyPrint.HughesPJClass

import qualified Kantour.QuotesFetch.QParser as QP
import Text.Megaparsec
import Control.Monad.State
import Data.Coerce

{-# ANN module "HLint: ignore Avoid lambda" #-}

processRegularQuotes :: IO ()
processRegularQuotes = do
    sdb <- getDatabase <$> fetchRawDatabase
    rqs <- fetchRawQuotes
    pages <- mapM (processPage sdb) rqs
    mapM_ (\page -> print (renderAll sdb kc3Table page)) pages

demoProcessRegularPage :: IO ()
demoProcessRegularPage = do
    content <- fetchWikiLink "大鲸"
    let Right (Page result) = parse QP.pScanAll "" content
        (trs,xs) = evalState PP.processPage (coerce result)
    putStrLn (prettyShow trs)
    let ppr (k,vs) = putStrLn (prettyShow k) >> putStrLn (prettyShow vs)
    mapM_ ppr xs

demoProcessSeasonalPage :: IO ()
demoProcessSeasonalPage = do
    content <- fetchWikiLink "季节性/2017年情人节"
    let Right (Page result) = parse QP.pScanAll "" content
        qs = PP.processPage2 result
    putStrLn (prettyShow qs)

defaultMain :: IO ()
defaultMain = do
    result <- fetchWikiLink "Template:舰娘导航"
    let (Right x) = parse QP.pCollectLinks "" result
    mapM_ putStrLn x

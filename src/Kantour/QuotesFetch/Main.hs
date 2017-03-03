{-# LANGUAGE ScopedTypeVariables #-}
module Kantour.QuotesFetch.Main where

import Kantour.QuotesFetch.Fetch
import Kantour.QuotesFetch.ShipDatabase
import Kantour.QuotesFetch.Quotes

import Text.PrettyPrint.HughesPJClass

import qualified Kantour.QuotesFetch.QParser as QP
import Text.Megaparsec

{-# ANN module "HLint: ignore Avoid lambda" #-}

processRegularQuotes :: IO ()
processRegularQuotes = do
    sdb <- getDatabase <$> fetchRawDatabase
    rqs <- fetchRawQuotes
    pages <- mapM (processPage sdb) rqs
    mapM_ (\page -> print (renderAll sdb kc3Table page)) pages

defaultMain :: IO ()
defaultMain = do
    -- content <- fetchWikiLink "季节性/2017年情人节"
    content <- fetchWikiLink "大鲸"
    let Right result = parse QP.pScanAll "" content
    putStrLn (prettyShow result)

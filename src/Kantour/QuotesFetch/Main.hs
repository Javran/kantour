{-# LANGUAGE ScopedTypeVariables #-}
module Kantour.QuotesFetch.Main where

import Kantour.QuotesFetch.Fetch
import Kantour.QuotesFetch.ShipDatabase
import Kantour.QuotesFetch.Quotes

import Kantour.QuotesFetch.Types
import Kantour.QuotesFetch.Template

import qualified Kantour.QuotesFetch.QParser as QP
import Text.Megaparsec
import Data.Dynamic

{-# ANN module "HLint: ignore Avoid lambda" #-}

processRegularQuotes :: IO ()
processRegularQuotes = do
    sdb <- getDatabase <$> fetchRawDatabase
    rqs <- fetchRawQuotes
    pages <- mapM (processPage sdb) rqs
    mapM_ (\page -> print (renderAll sdb kc3Table page)) pages

defaultMain :: IO ()
defaultMain = do
    {-
    content <- fetchWikiLink "季节性/2017年情人节"
    let results :: [RawQuote]
        results =
              concatMap fst
            . filter (null . snd)
            $ readP_to_S pFullScanSeasonal content
        pprRq xs = mapM_ pprPair xs >> putStrLn "===="
        pprPair (k,v) = putStrLn $ k ++ ": " ++ v
    mapM_ pprRq results -}
    content <- fetchWikiLink "大鲸"
    let Right result = parse QP.pScanAll "" content
        ppr dyn
            | Just (a :: Template) <- fromDynamic dyn = print a
            | Just (a :: [TabberRow]) <- fromDynamic dyn = print a
            | Just (a :: QP.Header) <- fromDynamic dyn = print a
            | otherwise = putStrLn $ "Unknown: " ++ show dyn
    mapM_ ppr result

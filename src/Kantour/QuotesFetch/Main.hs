{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}
module Kantour.QuotesFetch.Main where

import Kantour.QuotesFetch.Parser
import Kantour.QuotesFetch.Types
import Kantour.QuotesFetch.Fetch
import Kantour.QuotesFetch.ShipDatabase

import Data.List
import Control.Concurrent.ParallelIO.Global
import Text.JSON

processLink :: ShipDatabase -> String -> IO (String, [QuotesSection])
processLink sdb linkName = do
    content <- fetchWikiLink linkName
    if "==舰娘属性==" `isInfixOf` content
      then do
        let (trs,results) = collectAll content
            ppr (k,v) = putStrLn $ k ++ ": " ++ v ++ " ==> " ++ show (findMasterId v sdb)
        mapM_ ppr trs
        pure (linkName, results)
      else pure (linkName, [])

dumpQuotes :: ShipDatabase -> IO ()
dumpQuotes sdb = do
    resp <- fetchWikiLink "Template:舰娘导航"
    let links = filter (not . notKanmusuLink) (extractLinks resp)
        testLinks = take 5 links
    results <- parallel (map (processLink sdb) testLinks)
    stopGlobalPool
    let results' = filter (not . null . snd) results
        ppr (n,qs) = do
            putStrLn $ "Name: " ++ n
            let ppr' (s, qts) = do
                    putStrLn $ "Section: " ++ s
                    pprQuotesList qts
            mapM_ ppr' qs
    mapM_ ppr results'
    -- writeFile "dump.json" (encode results')

defaultMain :: IO ()
defaultMain = do
    sdb <- fetchDatabase
    dumpQuotes sdb

pprQuotesList :: [Quotes] -> IO ()
pprQuotesList qts = do
    putStrLn "++++ begin listing"
    mapM_ pprQuotes qts
    putStrLn "---- end listing"
  where
    pprQuotes :: Quotes -> IO ()
    pprQuotes = mapM_ pprQuote

    pprQuote :: (String, String) -> IO ()
    pprQuote (k,v) = putStrLn $ "    " ++ k ++ ": " ++ v

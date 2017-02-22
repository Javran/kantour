{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}
module Main where

import qualified Data.Text as T

import Text.ParserCombinators.ReadP
import Parser
import Types
import Data.List
import Control.Concurrent.ParallelIO.Global
import Text.JSON
import Language.Lua.Parser
import Language.Lua.Syntax
import Fetch

processLink :: String -> IO (String, [QuotesSection])
processLink linkName = do
    content <- fetchWikiLink linkName
    if "==舰娘属性==" `isInfixOf` content
      then do
        let parsed = readP_to_S pFullScan content
            parsed2 = map fst . filter ((== []) . snd) $ parsed
        pure (linkName, parsed2)
      else pure (linkName, [])

dumpQuotes :: IO ()
dumpQuotes = do
    resp <- fetchWikiLink "Template:舰娘导航"
    let links = filter (not . notKanmusuLink) (extractLinks resp)
    results <- parallel (map processLink links)
    stopGlobalPool
    let results' = filter (not . null . snd) results
    writeFile "dump.json" (encode results')

main :: IO ()
main = do
    initializeFetcher
    -- dumpQuotes
    content <- fetchWikiLink "模块:舰娘数据"
    let Right (Block parsed _) = parseText chunk (T.pack content)
    print (parsed !! 1)

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

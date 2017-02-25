{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}
module Kantour.QuotesFetch.Main where

import Kantour.QuotesFetch.Fetch
import Kantour.QuotesFetch.ShipDatabase
import Kantour.QuotesFetch.Quotes
import Kantour.QuotesFetch.Types
import Language.Lua

defaultMain :: IO ()
defaultMain = do
    sdb <- fetchDatabase
    rqs <- fetchRawQuotes
    pages <- mapM (processPage sdb) rqs
    mapM_ (\page -> print (renderAll sdb kc3Table page)) pages

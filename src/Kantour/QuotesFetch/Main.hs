{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}
module Kantour.QuotesFetch.Main where

import Kantour.QuotesFetch.Fetch
import Kantour.QuotesFetch.ShipDatabase
import Kantour.QuotesFetch.Quotes

defaultMain :: IO ()
defaultMain = do
    sdb <- fetchDatabase
    rqs <- fetchRawQuotes
    mapM_ (processPage sdb) rqs
    pure ()

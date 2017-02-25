{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}
module Kantour.QuotesFetch.Main where

import Kantour.QuotesFetch.Types
import Kantour.QuotesFetch.Fetch
import Kantour.QuotesFetch.ShipDatabase

defaultMain :: IO ()
defaultMain = do
    sdb <- fetchDatabase
    fetchRawQuotes
    pure ()

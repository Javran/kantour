module Kantour.QuotesFetch.ProcessPage where

import Kantour.QuotesFetch.Fetch
import Kantour.QuotesFetch.ShipDatabase

import Text.PrettyPrint.HughesPJClass

import Kantour.QuotesFetch.QParser
import Kantour.QuotesFetch.Kcwiki
import Kantour.QuotesFetch.Types

import Control.Monad.State

fetchTabberRows :: State [Component] TabberRows
fetchTabberRows = do
    xs <- get
    case xs of
        CHeader (Header 2 "舰娘属性"):CTabber tbs:_ -> pure tbs
        (_:xs') -> put xs' >> fetchTabberRows
        [] -> error "fetchTabberRows: input exhausted"

fetchQuoteList :: State [Component] [QuoteLine]
fetchQuoteList = do
    xs <- get
    case xs of
        CTemplate TplQuoteListBegin {}:xs' -> do
            put xs'
            let isTplEnd (CTemplate TplEnd) = True
                isTplEnd _ = False
            ys <- state (break isTplEnd)
            let cleanup :: [Component] -> [QuoteLine]
                cleanup = concatMap (\c -> case c of
                                         CTemplate (TplQuote ql) -> [ql]
                                         _ -> [])
            case ys of
                CTemplate TplEnd:ys' -> put ys' >> pure (cleanup ys)
                _ -> error "fetchQuoteList: parse error"
        (_:xs') -> put xs' >> fetchQuoteList
        [] -> error "fetchQuoteList: input exhausted"

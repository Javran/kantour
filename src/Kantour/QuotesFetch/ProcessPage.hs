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

nextHeader3 :: State [Component] (Maybe Header)
nextHeader3 = do
    xs <- get
    case xs of
        CHeader h@(Header 3 _):xs' ->
            put xs' >> pure (Just h)
        _: xs' -> put xs' >> nextHeader3
        [] -> pure Nothing

getQuotes :: State [Component] [QuoteLine]
getQuotes = do
    xs <- get
    case xs of
        CHeader (Header 3 _):_ ->
            pure []
        CTemplate (TplQuote ql):xs' ->
            put xs' >> (ql:) <$> getQuotes
        _:xs' ->
            put xs' >> getQuotes
        [] -> pure []

getQuoteSection :: State [Component] (Maybe (Header, [QuoteLine]))
getQuoteSection = do
    mHdr <- nextHeader3
    case mHdr of
        Just hdr -> do
            qs <- getQuotes
            pure (Just (hdr,qs))
        Nothing -> pure Nothing

processPage :: State [Component] (TabberRows,[(Header,[QuoteLine])])
processPage = do
    trs <- fetchTabberRows
    qss <- fix $ \self -> do
        mqs <- getQuoteSection
        case mqs of
            Nothing -> pure []
            Just qs -> (qs:) <$> self
    pure (trs, qss)

processPage2 :: [Component] -> [QuoteLine]
processPage2 = concatMap check
  where
    check c = case c of
        CTemplate (TplQuote ql) -> pure ql
        _ -> []

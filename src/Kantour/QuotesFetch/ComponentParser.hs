module Kantour.QuotesFetch.ComponentParser
  ( parseShipInfoPage
  , parseSeasonalPage
  ) where

import Kantour.QuotesFetch.Kcwiki
import Kantour.QuotesFetch.Types

import Control.Monad.State

{-

ComponentParser consumes a sequence of page components,
"parses" it to produce more structured data ready to be processed

-}

getTabberRows :: State [Component] (Maybe TabberRows)
getTabberRows = do
    xs <- get
    case xs of
        CHeader (Header 2 "舰娘属性"):CTabber tbs:_ -> pure (Just tbs)
        (_:xs') -> put xs' >> getTabberRows
        [] -> pure Nothing

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

getShipInfoPage :: State [Component] (Maybe (TabberRows,[(Header,[QuoteLine])]))
getShipInfoPage = do
    mtrs <- getTabberRows
    case mtrs of
        Nothing -> pure Nothing
        Just trs -> do
            qss <- fix $ \self -> do
                mqs <- getQuoteSection
                case mqs of
                    Nothing -> pure []
                    Just qs -> (qs:) <$> self
            pure (Just (trs, qss))

parseShipInfoPage :: [Component] -> Maybe (TabberRows,[(Header,[QuoteLine])])
parseShipInfoPage = evalState getShipInfoPage

parseSeasonalPage :: [Component] -> [QuoteLine]
parseSeasonalPage = concatMap check
  where
    check c = case c of
        CTemplate (TplQuote ql) -> pure ql
        _ -> []

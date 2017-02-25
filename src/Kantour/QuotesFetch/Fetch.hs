{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Kantour.QuotesFetch.Fetch where

import Text.XML.HXT.Core hiding (when)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Tree.NTree.TypeDefs
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Data.ByteString.Builder
import Data.List
import Control.Monad

import Control.Concurrent.ParallelIO.Global
import Kantour.QuotesFetch.Parser
import Kantour.QuotesFetch.Types

endpoint :: String
endpoint = "http://zh.kcwiki.moe/api.php"

fetchURL :: String -> QueryText -> IO String
fetchURL url qt = do
    mgr <- getGlobalManager
    initReq <- parseRequest url
    let qs = LBS.toStrict . toLazyByteString . renderQueryText False $ qt
        req = initReq { queryString = qs }
    resp <- httpLbs req mgr
    let st = responseStatus resp
    if st == ok200
        then pure (T.unpack . T.decodeUtf8 . LBS.toStrict . responseBody $ resp)
        else fail $ "error with status code: " ++ show (statusCode st)

fetchWikiLink :: String -> IO String
fetchWikiLink wlink = do
    let qt = [ ("action", Just "query")
             , ("prop", Just "revisions")
             , ("rvprop", Just "content")
             , ("format", Just "xml")
             , ("titles", Just (T.pack wlink))
             , ("converttitles", Nothing)
             , ("redirects", Nothing)
             ]
    getRevisionsContent <$> fetchURL endpoint qt
  where
    getRevisionsContent :: String -> String
    getRevisionsContent raw = content
      where
        -- xreadDoc is like xread but reads the XML spec
        [NTree (XText content) _] = runLA
            (xreadDoc >>> isElem
             >>> deep (hasName "revisions" /> hasName "rev")
             >>> getChildren) raw

fetchRawQuotesByLink :: String
                     -> IO (LinkName, Maybe RawPage)
fetchRawQuotesByLink linkName = do
    content <- fetchWikiLink linkName
    pure . (linkName,) $ do
        guard $ "==舰娘属性==" `isInfixOf` content
        let page = collectAll content
        pure page

fetchRawQuotes :: IO [(LinkName, RawPage)]
fetchRawQuotes = do
    resp <- fetchWikiLink "Template:舰娘导航"
    let links = filter (not . notKanmusuLink) (extractLinks resp)
        -- testLinks = take 5 links
        testLinks = links
    results <- parallel (map fetchRawQuotesByLink testLinks)
    stopGlobalPool
    let (ls,rs) =
            partitionEither $
                map (\(l,m) -> maybe (Left l) (Right . (l,)) m)
                    results
    putStrLn "Fetched nothing from following links:"
    putStrLn (intercalate ", " ls)
    pure rs

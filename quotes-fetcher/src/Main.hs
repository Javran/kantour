{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , ScopedTypeVariables
  , LambdaCase
  #-}
module Main where

import Network.HTTP.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad.State hiding (get)
import Control.Monad.Catch
import Text.XML.HXT.Core hiding (when)
import Data.Tree.NTree.TypeDefs

import Data.ByteString.Builder
import Text.ParserCombinators.ReadP
import Parser
import Data.List
import Control.Concurrent.ParallelIO.Global
import Text.JSON

data QFState = QFS
  { qfManager :: Manager
  }

newtype QuoteFetch a = QF (StateT QFState IO a)
  deriving
  ( MonadState QFState
  , MonadIO
  , MonadThrow
  , Monad
  , Applicative
  , Functor
  )

endpoint :: String
endpoint = "http://zh.kcwiki.moe/api.php"

fetchURL :: String -> QueryText -> QuoteFetch T.Text
fetchURL url qt = do
    mgr <- gets qfManager
    initReq <- parseRequest url
    let qs = LBS.toStrict . toLazyByteString . renderQueryText False $ qt
        req = initReq { queryString = qs }
    resp <- liftIO (httpLbs req mgr)
    let st = responseStatus resp
    if st == ok200
        then pure (T.decodeUtf8 . LBS.toStrict . responseBody $ resp)
        else fail $ "error with status code: " ++ show (statusCode st)

fetchWikiLink :: String -> QuoteFetch T.Text
fetchWikiLink wlink = do
    let qt = [ ("action", Just "query")
             , ("prop", Just "revisions")
             , ("rvprop", Just "content")
             , ("format", Just "xml")
             , ("titles", Just (T.pack wlink))
             , ("converttitles", Nothing)
             , ("redirects", Nothing)
             ]
    fetchURL endpoint qt

runQF :: QuoteFetch a -> QFState -> IO a
runQF (QF m) = evalStateT m

getRevisionsContent :: T.Text -> String
getRevisionsContent raw = content
  where
    raw' = T.unpack raw
    -- xreadDoc is like xread but reads the XML spec
    [NTree (XText content) _] = runLA
        (xreadDoc >>> isElem
         >>> deep (hasName "revisions" /> hasName "rev")
         >>> getChildren) raw'

processLink :: Manager -> String -> IO (String, [QuotesSection])
processLink mgr linkName = do
    resp <- runQF (fetchWikiLink linkName) (QFS mgr)
    let content = getRevisionsContent resp
    if "==舰娘属性==" `isInfixOf` content
      then do
        let parsed = readP_to_S pFullScan content
            parsed2 = map fst . filter ((== []) . snd) $ parsed
        -- mapM_ (\(h,q) -> putStrLn ("header: " ++ h) >> pprQuotesList q) parsed2
        pure (linkName, parsed2)
      else pure (linkName, [])

main :: IO ()
main = do
    mgr <- newManager tlsManagerSettings
    resp <- runQF (fetchWikiLink "Template:舰娘导航") (QFS mgr)
    let resp' = T.unpack resp
        links = filter (not . notKanmusuLink) (extractLinks resp')
    results <- parallel (map (processLink mgr) links)
    stopGlobalPool
    let results' = filter (not . null . snd) results
    writeFile "dump.json" (encode results')
    pure ()

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

{-

  (String, [QuotesSection])

- String for link name

- QuotesSection = (String, [Quotes])

    - String for header name
    - [Quotes] a list of quotes

    - a single Quotes consists of some key-value bindings

-}

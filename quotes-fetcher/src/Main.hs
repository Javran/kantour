{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Network.HTTP.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Control.Monad.State
import Control.Monad.Catch
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs

import Data.ByteString.Builder
import Data.Default
import Text.Pandoc.Readers.MediaWiki
import Text.Pandoc.Walk
import Text.Pandoc.Definition

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

fetchURLContent :: String -> QueryText -> QuoteFetch T.Text
fetchURLContent url qt = do
    mgr <- gets qfManager
    initReq <- parseRequest url
    let qs = LBS.toStrict . toLazyByteString . renderQueryText False $ qt
        req = initReq { queryString = qs }
    resp <- liftIO (httpLbs req mgr)
    let st = responseStatus resp
    if st == ok200
        then pure (T.decodeUtf8 . LBS.toStrict . responseBody $ resp)
        else fail $ "error with status code: " ++ show (statusCode st)

runQF :: QuoteFetch a -> QFState -> IO a
runQF (QF m) = evalStateT m

main :: IO ()
main = do
    mgr <- newManager tlsManagerSettings
    let qt = [ ("action", Just "query")
             , ("prop", Just "revisions")
             , ("rvprop", Just "content")
             , ("format", Just "xml")
             , ("titles", Just "Template:舰娘导航")
             ]
    resp <- runQF (fetchURLContent endpoint qt) (QFS mgr)
    let resp' = T.unpack resp
    [NTree (XText content) _] <- runX (readString [] resp'
                    >>> deep (hasName "revisions" /> hasName "rev")
                    >>> getChildren)
    let Right content' = readMediaWiki def content
        linksRaw = query (\ (inline :: Inline) -> case inline of
                          Link {} -> [inline]
                          _ -> []) content'
        links = map (\(Link _ _ (t,_)) -> t) linksRaw
    mapM_ putStrLn links

module Kantour.GameServerLab
  ( SubCmdGameServerLab
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe
import Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.Functor
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import Data.Time.Clock
import Kantour.Core.GameResource.Magic (servers)
import Kantour.Subcommand
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Network.HTTP.Date as HD

{-
  Proof of concept stuff,
  first create a script to download stuff from all kc servers.

  note: can use HEAD on /kcs2/js/main.js

 -}

data SubCmdGameServerLab

instance Subcommand SubCmdGameServerLab where
  name _ = "GameServerLab"
  main _ = defaultMain

fetchResource :: Manager -> String -> IO (Either SomeException (Maybe Int, Maybe UTCTime))
fetchResource mgr serverAddr = catchAny fetch' (pure . Left)
  where
    fetch' = do
      req <- do
        r0 <- parseRequest $ "http://" <> serverAddr <> "/kcs2/js/main.js"
        pure $
          r0
            { method = "HEAD"
            , checkResponse = throwErrorStatusCodes
            , requestHeaders = [("Accept-Encoding", "")]
            }
      resp <- httpLbs req mgr
      let respHs = responseHeaders resp
          cl = do
            raw <- lookup "Content-Length" respHs
            Right v <- pure $ P.parseOnly (P.decimal @Int) raw
            pure v
          lm =
            lookup "Last-Modified" respHs
              >>= HD.parseHTTPDate
              <&> HD.httpDateToUTC
      pure $ Right (cl, lm)

fetchResourceWithRetries :: Manager -> String -> Int -> IO (Maybe (Maybe Int, Maybe UTCTime), [SomeException])
fetchResourceWithRetries mgr serverAddr retries =
  if retries <= 0
    then pure (Nothing, [])
    else
      fetchResource mgr serverAddr >>= \case
        Right v -> pure (Just v, [])
        Left e -> do
          threadDelay $ 1000 * 100
          (v, rs) <- fetchResourceWithRetries mgr serverAddr (retries - 1)
          pure (v, e : rs)

defaultMain :: IO ()
defaultMain = do
  mgr <- newTlsManager
  forM_ (IM.toAscList servers) \(k, v) -> do
    putStrLn $ "Server #" <> show k
    r <- fetchResourceWithRetries mgr (T.unpack v) 4
    print r

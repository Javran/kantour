module Kantour.GameServerLab
  ( SubCmdGameServerLab
  ) where

import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import Kantour.Core.GameResource.Magic (servers)
import Kantour.Subcommand
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)

{-
  Proof of concept stuff,
  first create a script to download stuff from all kc servers.

  note: can use HEAD on /kcs2/js/main.js

 -}

data SubCmdGameServerLab

instance Subcommand SubCmdGameServerLab where
  name _ = "GameServerLab"
  main _ = defaultMain

defaultMain :: IO ()
defaultMain = do
  mgr <- newTlsManager
  let s1 = servers IM.! 1
  req <- do
    r0 <- parseRequest $ "http://" <> T.unpack s1 <> "/kcs2/js/main.js"
    pure $
      r0
        { method = "HEAD"
        , checkResponse = throwErrorStatusCodes
        , requestHeaders = [("Accept-Encoding", "")]
        }
  resp <- httpLbs req mgr
  print req
  let respHs = responseHeaders resp
      cl = do
        raw <- lookup "Content-Length" respHs
        Right v <- pure $ P.parseOnly (P.decimal @Integer) raw
        pure v
  print $ lookup "Last-Modified" respHs
  print cl

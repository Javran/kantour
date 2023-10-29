module Kantour.GameServerLab
  ( SubCmdGameServerLab
  ) where

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
  mapM_ print $ responseHeaders resp
  print $ lookup "Last-Modified" (responseHeaders resp)

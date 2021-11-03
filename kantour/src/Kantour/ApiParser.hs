{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Kantour.ApiParser
  ( SubCmdApiParser
  )
where

import Control.Monad
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.List
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Vector as V
import Kantour.Core.KcData.Master.Common
import Kantour.Core.KcData.Master.Root
import Kantour.FetchMasterData
import Kantour.Subcommand
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Shower
import System.Environment
import System.Exit

data SubCmdApiParser

instance Subcommand SubCmdApiParser where
  name _ = "ApiParser"
  main _ = defaultMain

{-
  TODO:
  We need a test suite that collects for each type and verify that
  we have left nothing unconsumed.
 -}
defaultMain :: IO ()
defaultMain =
  getArgs >>= \case
    [fileOrUrlSrc] -> do
      mgr <- newManager tlsManagerSettings
      rawJson <- loadFromSource mgr fileOrUrlSrc
      CollectExtra {ceValue = r, ceExtra} <-
        case Aeson.eitherDecode @(CollectExtra MasterRoot) rawJson of
          Left msg -> die ("parse error: " <> msg)
          Right r -> pure r
      putStrLn "Ship sample:"
      printer (head $ mstShip r)
      unless (null ceExtra) $ do
        putStrLn "Following fields are not yet accounted for:"
        forM_ (sortOn fst ceExtra) $ \(k, v) -> do
          putStrLn $ "- " <> T.unpack k
          let valToStr = T.unpack . decodeUtf8 . BSL.toStrict . encode
          case v of
            Array xs -> do
              putStrLn "Value is an array, showing first 2 elements:"
              mapM_ (putStrLn . valToStr) (V.take 2 xs)
            _ -> do
              putStrLn "Truncated sample: "
              putStrLn (take 2000 $ valToStr v)
    _ -> die "<subcmd> [http]<source>"

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Kantour.Core.KcData.Master.CompletenessSpec
  ( spec
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Picker
import qualified Data.ByteString.Lazy as BSL
import Data.Containers.ListUtils
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import Kantour.Core.DataFiles
import Kantour.Core.KcData.Master.Common
import Kantour.Core.KcData.Master.Ship
import Test.Hspec

loadMaster :: IO Value
loadMaster =
  loadDataFile "data/api_start2.json.xz" >>= \raw ->
    case eitherDecode raw of
      Left msg -> fail $ "Cannot parse data: " <> msg
      Right v -> pure v

{-
  This module verifies that all fields in source master data
  are being recognized properly.
 -}

spec :: Spec
spec = describe "Completeness" $
  before loadMaster $
    specify "Ship" $ \rawMst -> do
      let xs = (rawMst |-- ["api_mst_ship"] :: [Value])
          ys :: [CollectExtra Ship]
          ys = fmap (getResult . fromJSON) xs
            where
              getResult = \case
                Error msg -> error msg
                Success v -> v
          unknownFields = M.fromListWith (<>) $ do
            CollectExtra {ceExtra} <- ys
            (k, v) <- ceExtra
            pure (k, [v])
      if unknownFields == M.empty
        then () `shouldBe` ()
        else do
          liftIO $ do
            putStrLn "Unknown fields detected for Ship:"
            forM_ (M.toAscList unknownFields) $ \(k, vsPre) -> do
              let vs = take 5 $ nubOrd $ fmap encode vsPre
              T.putStrLn $ "  Samples for " <> k <> ":"
              forM_ vs $ \v ->
                T.putStrLn $ "  - " <> decodeUtf8 (BSL.toStrict v)
          pendingWith $
            "Unknown fields: " <> unwords (T.unpack <$> M.keys unknownFields)

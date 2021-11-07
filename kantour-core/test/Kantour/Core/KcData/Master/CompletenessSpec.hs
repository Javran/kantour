{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.Proxy
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import GHC.Generics
import Kantour.Core.DataFiles
import Kantour.Core.KcData.Master.Common
import qualified Kantour.Core.KcData.Master.Ship as Ship
import qualified Kantour.Core.KcData.Master.Slotitem as Slotitem
import Test.Hspec

loadMaster :: IO Value
loadMaster =
  loadDataFile "data/api_start2.json.xz" >>= \raw ->
    case eitherDecode raw of
      Left msg -> fail $ "Cannot parse data: " <> msg
      Right v -> pure v

{-
  from: https://stackoverflow.com/a/48179707/315302
 -}
datatyName :: (HasDatatype (Rep a), Generic a) => a -> String
datatyName = gDatatyName . from

class HasDatatype (f :: * -> *) where
  gDatatyName :: f x -> String

instance Datatype c => HasDatatype (M1 D c f) where
  gDatatyName = datatypeName

{-
  This module verifies that all fields in source master data
  are being recognized properly.
 -}

spec :: Spec
spec = describe "Completeness" $
  before loadMaster $ do
    let mkTest
          :: forall p a.
          (FromJSON a, HasKnownFields a, Generic a, HasDatatype (Rep a))
          => p a
          -> [T.Text]
          -> SpecWith Value
        mkTest _ty selector =
          let dName = datatyName (undefined :: a)
           in specify dName $ \rawMst -> do
                let xs = (rawMst |-- selector :: [Value])
                    ys :: [CollectExtra a]
                    ys = fmap (getResult . fromJSON @(CollectExtra a)) xs
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
                      putStrLn $ "Unknown fields detected for " <> dName <> ":"
                      forM_ (M.toAscList unknownFields) $ \(k, vsPre) -> do
                        let vs = take 5 $ nubOrd $ fmap encode vsPre
                        T.putStrLn $ "  Samples for " <> k <> ":"
                        forM_ vs $ \v ->
                          T.putStrLn $ "  - " <> decodeUtf8 (BSL.toStrict v)
                    pendingWith $
                      "Unknown fields: " <> unwords (T.unpack <$> M.keys unknownFields)

    mkTest (Proxy @Ship.Ship) ["api_mst_ship"]
    mkTest (Proxy @Slotitem.Slotitem) ["api_mst_slotitem"]

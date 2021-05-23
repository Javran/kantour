{-# LANGUAGE OverloadedStrings #-}

module Kantour.KcData.Map.Xywh where

import Data.Aeson

newtype Xy a = Xy (a, a)

newtype Wh a = Wh (a, a)

newtype Xywh a = Xywh (Xy a, Wh a)

instance FromJSON a => FromJSON (Xy a) where
  parseJSON = withObject "Xy" $ \o ->
    Xy <$> do
      (,) <$> (o .: "x") <*> (o .: "y")

instance FromJSON a => FromJSON (Wh a) where
  parseJSON = withObject "Wh" $ \o ->
    Wh <$> do
      (,) <$> (o .: "w") <*> (o .: "h")

instance FromJSON a => FromJSON (Xywh a) where
  parseJSON = withObject "Xywh" $ \o ->
    Xywh <$> do
      (,)
        <$> parseJSON (Object o)
        <*> parseJSON (Object o)

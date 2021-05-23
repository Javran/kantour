{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Kantour.KcData.Map.BgObject where

import Control.Applicative
import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data BgObject = BgObject
  { img :: T.Text
  , name :: Maybe T.Text
  }
  deriving (Generic, Show)

instance FromJSON BgObject where
  parseJSON v =
    withText
      "BgObject.Str"
      (\img ->
         pure $
           BgObject
             { img
             , name = Nothing
             })
      v
      <|> withObject
        "BgObject.Obj"
        (\o -> do
           img <- o .: "img"
           name <- o .: "name"
           pure $ BgObject {img, name})
        v

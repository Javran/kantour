{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Kantour.KcData.Map.Background where

import Control.Applicative
import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data Background = Background
  { img :: T.Text
  , name :: Maybe T.Text
  }
  deriving (Generic, Show)

instance FromJSON Background where
  parseJSON v =
    withText
      "Background.Str"
      (\img ->
         pure $
           Background
             { img
             , name = Nothing
             })
      v
      <|> withObject
        "Background.Obj"
        (\o -> do
           img <- o .: "img"
           name <- o .: "name"
           pure $ Background {img, name})
        v

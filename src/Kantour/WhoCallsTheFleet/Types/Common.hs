{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Kantour.WhoCallsTheFleet.Types.Common where

import qualified Data.Vector as V
import Control.Monad
import Data.Aeson
import Data.Aeson.Types

data Resource a = Resource
  { fuel :: a
  , ammo :: a
  , steel :: a
  , bauxite :: a
  }

parseResource :: FromJSON a => Value -> Parser (Resource a)
parseResource = withArray "Resource" $ \arr -> do
    guard $ V.length arr == 4
    Resource
        <$> parseJSON (arr V.! 0)
        <*> parseJSON (arr V.! 1)
        <*> parseJSON (arr V.! 2)
        <*> parseJSON (arr V.! 3)

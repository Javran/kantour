module Kantour.WhoCallsTheFleet.Types.Common where

import Control.Monad
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics

data Resource a = Resource
  { fuel :: a
  , ammo :: a
  , steel :: a
  , bauxite :: a
  }
  deriving (Show, Generic)

instance FromJSON a => FromJSON (Resource a) where
  parseJSON =
    withArray "Resource" $ \arr -> do
      guard $ V.length arr == 4
      Resource
        <$> parseJSON (arr V.! 0)
        <*> parseJSON (arr V.! 1)
        <*> parseJSON (arr V.! 2)
        <*> parseJSON (arr V.! 3)

data Name = Name
  { jaJP :: T.Text
  , jaKana :: T.Text
  , jaRomaji :: T.Text
  , zhCN :: T.Text
  , suffix :: Maybe Int
  }
  deriving (Generic, Show)

instance FromJSON Name where
  parseJSON = withObject "Name" $ \v ->
    Name
      <$> v .: "ja_jp"
      <*> v .: "ja_kana"
      <*> v .: "ja_romaji"
      <*> v .: "zh_cn"
      <*> v .:? "suffix"

{-# LANGUAGE DeriveGeneric #-}
module Kantour.Core.KcData.Map.Image where

import Data.Aeson
import GHC.Generics
import qualified Data.HashMap.Strict as HM
import Kantour.Core.KcData.Map.Sprite
import qualified Data.Text as T

data Image = Image
  { frames :: HM.HashMap T.Text Sprite
  , meta :: Maybe Value
  } deriving (Generic)

instance FromJSON Image

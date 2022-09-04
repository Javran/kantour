module Kantour.Core.KcData.Map.Image where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import GHC.Generics
import Kantour.Core.KcData.Map.Sprite

data Image = Image
  { frames :: HM.HashMap T.Text Sprite
  , meta :: Maybe Value
  }
  deriving (Generic)

instance FromJSON Image

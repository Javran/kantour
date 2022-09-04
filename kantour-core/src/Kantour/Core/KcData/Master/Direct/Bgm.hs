{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Direct.Bgm (
  Bgm (..),
) where

import Control.DeepSeq (NFData)
import Data.Aeson as Aeson
import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Direct.Common

data Bgm = Bgm
  { bgmId :: Int
  , name :: T.Text
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier (Rename "bgmId" "id" : KcConvention)]
          Bgm

instance NFData Bgm

instance HasKnownFields Bgm where
  knownFields _ = kcFields "id name"

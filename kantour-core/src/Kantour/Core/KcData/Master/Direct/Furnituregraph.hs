{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Direct.Furnituregraph (
  Furnituregraph (..),
) where

import Control.DeepSeq (NFData)
import Data.Aeson as Aeson
import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Direct.Common

data Furnituregraph = Furnituregraph
  { version :: T.Text
  , filename :: T.Text
  , fgType :: Int
  , fgId :: Int
  , no :: Int
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[ FieldLabelModifier
              ( Rename "fgType" "type"
                  : Rename "fgId" "id" : KcConvention
              )
           ]
          Furnituregraph

instance NFData Furnituregraph

instance HasKnownFields Furnituregraph where
  knownFields _ =
    kcFields
      "version filename type id no"

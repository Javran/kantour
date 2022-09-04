{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Furnituregraph (
  Furnituregraph (..),
) where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Common

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

instance HasKnownFields Furnituregraph where
  knownFields _ =
    kcFields
      "version filename type id no"

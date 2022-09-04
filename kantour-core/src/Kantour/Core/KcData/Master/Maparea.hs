{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Maparea (
  Maparea (..),
) where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Common

data Maparea = Maparea
  { mType :: Int
  , mId :: Int
  , name :: T.Text
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[ FieldLabelModifier
              ( Rename "mType" "type"
                  : Rename "mId" "id" : KcConvention
              )
           ]
          Maparea

instance HasKnownFields Maparea where
  knownFields _ = kcFields "type id name"

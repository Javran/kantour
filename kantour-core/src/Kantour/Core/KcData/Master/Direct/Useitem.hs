{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Direct.Useitem (
  Useitem (..),
) where

import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Direct.Common

data Useitem = Useitem
  { usetype :: Int
  , category :: Int
  , price :: Int
  , uId :: Int
  , name :: T.Text
  , description :: [T.Text]
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[ FieldLabelModifier (Rename "uId" "id" : KcConvention)
           ]
          Useitem

instance HasKnownFields Useitem where
  knownFields _ =
    kcFields
      "usetype category price id name description"

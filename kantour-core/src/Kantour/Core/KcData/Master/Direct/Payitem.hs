{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Direct.Payitem (
  Payitem (..),
) where

import Control.DeepSeq (NFData)
import Data.Aeson as Aeson
import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Direct.Common

data Payitem = Payitem
  { price :: Int
  , pType :: Int
  , pId :: Int
  , item :: [Int]
  , shopDescription :: T.Text
  , name :: T.Text
  , description :: T.Text
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[ FieldLabelModifier
              ( Rename "pType" "type"
                  : Rename "pId" "id"
                    : KcConvention
              )
           ]
          Payitem
instance NFData Payitem
instance HasKnownFields Payitem where
  knownFields _ =
    kcFields "price type id item shop_description name description"

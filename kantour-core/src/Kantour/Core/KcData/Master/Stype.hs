{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Kantour.Core.KcData.Master.Stype
  ( Stype (..)
  )
where

import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Common
import qualified Data.HashMap.Strict as HM

data Stype = Stype
  { kcnt :: Int
  , sortno :: Int
  , scnt :: Int
  , equipType :: HM.HashMap T.Text Int
  , name :: T.Text
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[ FieldLabelModifier KcConvention
           ]
          Stype

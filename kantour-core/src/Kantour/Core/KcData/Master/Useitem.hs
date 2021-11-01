{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Useitem
  ( Useitem (..)
  )
where

import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Common

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

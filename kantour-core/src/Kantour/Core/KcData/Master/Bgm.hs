{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Bgm
  ( Bgm (..)
  )
where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Common

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

instance HasKnownFields Bgm where
  knownFields _ = kcFields "id name"

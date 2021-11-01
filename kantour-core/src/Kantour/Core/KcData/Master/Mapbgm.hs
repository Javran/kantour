{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Mapbgm
  ( Mapbgm (..)
  )
where

import Data.Aeson as Aeson
import Deriving.Aeson
import Kantour.Core.KcData.Master.Common

data Mapbgm = Mapbgm
  { movingBgm :: Int
  , mapBgm :: [Int]
  , mId :: Int
  , bossBgm :: [Int]
  , mapareaId :: Int
  , no :: Int
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[ FieldLabelModifier
               ( Rename "mId" "id" : KcConvention
               )
           ]
          Mapbgm

instance HasKnownFields Mapbgm where
  knownFields _ = kcFields "moving_bgm map_bgm id boss_bgm maparea_id no"

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators, OverloadedStrings #-}

module Kantour.Core.KcData.Master.Shipgraph
  ( Shipgraph (..)
  )
where

import Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Common

data Shipgraph = Shipgraph
  { ensyueN :: Maybe [Int]
  , kaisyuN :: Maybe [Int]
  , version :: NE.NonEmpty T.Text
  , kaisyuD :: Maybe [Int]
  , wedb :: Maybe [Int]
  , ensyufD :: Maybe [Int]
  , battleD :: Maybe [Int]
  , filename :: T.Text
  , pab :: Maybe [Int]
  , sortno :: Maybe Int
  , ensyufN :: Maybe [Int]
  , battleN :: Maybe [Int]
  , bokoN :: Maybe [Int]
  , shipId :: Int
  , mapN :: Maybe [Int]
  , kaizoD :: Maybe [Int]
  , bokoD :: Maybe [Int]
  , weda :: Maybe [Int]
  , mapD :: Maybe [Int]
  , kaizoN :: Maybe [Int]
  , pa :: Maybe [Int]
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier (Rename "shipId" "id" : KcConvention)]
          Shipgraph
instance HasKnownFields Shipgraph where
  knownFields _ =
    kcFields
      "ensyue_n kaisyu_n version kaisyu_d wedb ensyuf_d battle_d \
      \filename pab sortno ensyuf_n battle_n boko_n id \
      \map_n kaizo_d boko_d weda map_d kaizo_n pa"

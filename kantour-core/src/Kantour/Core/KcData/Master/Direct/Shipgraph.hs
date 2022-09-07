{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.Shipgraph (
  Shipgraph (..),
) where

import Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import Kantour.Core.KcData.Master.Direct.Common

data Shipgraph = Shipgraph
  { battleD :: Maybe [Int]
  , battleN :: Maybe [Int]
  , bokoD :: Maybe [Int]
  , bokoN :: Maybe [Int]
  , ensyueN :: Maybe [Int]
  , ensyufD :: Maybe [Int]
  , ensyufN :: Maybe [Int]
  , filename :: T.Text
  , kaisyuD :: Maybe [Int]
  , kaisyuN :: Maybe [Int]
  , kaizoD :: Maybe [Int]
  , kaizoN :: Maybe [Int]
  , mapD :: Maybe [Int]
  , mapN :: Maybe [Int]
  , pa :: Maybe [Int]
  , pab :: Maybe [Int]
  , kcId :: Int
  , sortno :: Maybe Int
  , version :: NE.NonEmpty T.Text
  , weda :: Maybe [Int]
  , wedb :: Maybe [Int]
  }
  deriving stock (Generic, Show)

instance FromJSON Shipgraph where
  parseJSON = parseKcMstJson

instance NFData Shipgraph
instance HasKnownFields Shipgraph where
  knownFields _ =
    kcFields
      "ensyue_n kaisyu_n version kaisyu_d wedb ensyuf_d battle_d \
      \filename pab sortno ensyuf_n battle_n boko_n id \
      \map_n kaizo_d boko_d weda map_d kaizo_n pa"

instance Verifiable Shipgraph where
  verify
    Shipgraph
      { kcId
      , sortno
      , battleD
      , battleN
      , bokoD
      , bokoN
      , ensyueN
      , ensyufD
      , ensyufN
      , kaisyuD
      , kaisyuN
      , kaizoD
      , kaizoN
      , mapD
      , mapN
      , pa
      , pab
      , weda
      , wedb
      , version
      } = fix \(_ :: m ()) -> do
      let warn msg = vLogS $ "Shipgraph{" <> show kcId <> "}: " <> msg
          noJust :: (Show a, Eq a) => String -> Maybe a -> m ()
          noJust tag var = do
            when (isJust var) do
              warn $ tag <> " should be Nothing: " <> show var
          justShaped tag var = case var of
            Just [_, _] -> pure ()
            _ -> warn $ tag <> " has wrong shape: " <> show var
      when (length version /= 3) do
        warn "version length should be 3"
      unless (all isIntParsable version) do
        warn "some version elememts are not parsable as int"
      -- bounds are based on `ShipUtil > isEnemy`
      when (kcId <= 1500) do
        justShaped "battleD" battleD
        justShaped "battleN" battleN
        when (isNothing sortno) do
          warn "sortno is Nothing"
        justShaped "bokoD" bokoD
        justShaped "bokoN" bokoN
        justShaped "ensyueN" ensyueN
        justShaped "ensyufD" ensyufD
        justShaped "ensyufN" ensyufN
        justShaped "kaisyuD" kaisyuD
        justShaped "kaisyuN" kaisyuN
        justShaped "kaizoD" kaizoD
        justShaped "kaizoN" kaizoN
        justShaped "mapD" mapD
        justShaped "mapN" mapN
        justShaped "pa" pa
        justShaped "pab" pab
        justShaped "weda" weda
        justShaped "wedb" wedb

      when (kcId > 1500) do
        -- only "battleD" and "battleN" should exist.
        noJust "sortno" sortno
        noJust "bokoD" bokoD
        noJust "bokoN" bokoN
        noJust "ensyueN" ensyueN
        noJust "ensyufD" ensyufD
        noJust "ensyufN" ensyufN
        noJust "kaisyuD" kaisyuD
        noJust "kaisyuN" kaisyuN
        noJust "kaizoD" kaizoD
        noJust "kaizoN" kaizoN
        noJust "mapD" mapD
        noJust "mapN" mapN
        noJust "pa" pa
        noJust "pab" pab
        noJust "weda" weda
        noJust "wedb" wedb

      when (kcId > 5000) do
        -- those appears to be seasonal CGs.
        noJust "battleD" battleD
        noJust "battleN" battleN

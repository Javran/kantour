{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.Mapbgm (
  Mapbgm (..),
) where

import Data.Aeson as Aeson
import Kantour.Core.KcData.Master.Direct.Common

data Mapbgm = Mapbgm
  { movingBgm :: Int
  , mapBgm :: [Int]
  , kcId :: Int
  , bossBgm :: [Int]
  , mapareaId :: Int
  , no :: Int
  }
  deriving stock (Generic, Show)

instance FromJSON Mapbgm where
  parseJSON = parseKcMstJson

instance NFData Mapbgm

instance HasKnownFields Mapbgm where
  knownFields _ = kcFields "moving_bgm map_bgm id boss_bgm maparea_id no"

instance Verifiable Mapbgm where
  verify Mapbgm {kcId, mapareaId, no, mapBgm, bossBgm} = do
    let warn msg = vLogS $ "Mapbgm{" <> show kcId <> "}: " <> msg
    when (kcId /= mapareaId * 10 + no) do
      warn "wrong mapareaId & no combination"
    case mapBgm of
      [_, _] -> pure ()
      xs -> warn $ "mapBgm expect exactly 2:" <> show xs
    case bossBgm of
      [_, _] -> pure ()
      xs -> warn $ "bossBgm expect exactly 2:" <> show xs

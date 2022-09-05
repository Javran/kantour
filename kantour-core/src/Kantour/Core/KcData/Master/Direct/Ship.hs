{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Kantour.Core.KcData.Master.Direct.Ship (
  Ship (..),
) where

import Control.DeepSeq (NFData)
import Data.Aeson as Aeson
import Data.Ix (inRange)
import Data.Maybe
import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Direct.Common
import Data.Char

data Ship = Ship
  { shipId :: Int
  , sortno :: Maybe Int
  , sortId :: Int
  , name :: T.Text
  , yomi :: T.Text
  , stype :: Int
  , ctype :: Int
  , bullMax :: Maybe Int
  , fuelMax :: Maybe Int
  , taik :: Maybe [Int]
  , souk :: Maybe [Int]
  , houg :: Maybe [Int]
  , raig :: Maybe [Int]
  , tyku :: Maybe [Int]
  , tais :: Maybe [Int]
  , luck :: Maybe [Int]
  , soku :: Int
  , leng :: Maybe Int
  , slotNum :: Int
  , maxeq :: Maybe [Int]
  , buildtime :: Maybe Int
  , broken :: Maybe [Int]
  , powup :: Maybe [Int]
  , backs :: Maybe Int
  , getmes :: Maybe T.Text
  , afterlv :: Maybe Int
  , aftershipid :: Maybe T.Text
  , afterfuel :: Maybe Int
  , afterbull :: Maybe Int
  , voicef :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier (Rename "shipId" "id" : KcConvention)]
          Ship
instance NFData Ship
instance HasKnownFields Ship where
  knownFields _ =
    kcFields
      "id ctype name slot_num sort_id tyku buildtime leng \
      \backs voicef houg getmes bull_max fuel_max luck \
      \afterfuel aftershipid afterbull sortno yomi souk broken \
      \taik afterlv soku stype maxeq powup raig tais"

instance Verifiable Ship where
  verify
    Ship
      { shipId
      , name
      , sortno
      , bullMax
      , fuelMax
      , taik
      , souk
      , houg
      , raig
      , tyku
      , luck
      , leng
      , maxeq
      , buildtime
      , broken
      , powup
      , backs
      , getmes
      , afterlv
      , aftershipid
      , afterfuel
      , afterbull
      , voicef
      } = do
      let warn msg = vLogS $ "Ship{" <> T.unpack name <> "," <> show shipId <> "}: " <> msg
          isJust', isNothing' :: (Show a, Eq a) => String -> Maybe a -> _
          isJust' tag var = unless (isJust var) do
            warn $ tag <> " should be Just"
          expect :: (Show a, Eq a) => String -> a -> a -> _
          expect tag var e = unless (var == e) do
            warn $ tag <> " should be " <> show e <> ", got: " <> show var
          isNothing' tag var = expect tag var Nothing

      if shipId <= 1500
        then do
          isJust' "sortno" sortno
          isJust' "fuelMax" fuelMax
          isJust' "bullMax" bullMax

          isJust' "taik" taik
          isJust' "souk" souk
          isJust' "houg" houg
          isJust' "raig" raig
          isJust' "tyku" tyku

          isJust' "luck" luck

          isJust' "maxeq" maxeq
          isJust' "buildtime" buildtime
          isJust' "broken" broken
          isJust' "powup" powup
          isJust' "backs" backs

          case afterlv of
            Nothing -> do
              isNothing' "aftershipid" aftershipid
              isNothing' "afterfuel" afterfuel
              isNothing' "afternull" afterbull
            Just _ -> do
              case aftershipid of
                Nothing -> warn "aftershipid should be Just"
                Just x -> when (T.null x || not (T.all isDigit x)) do
                  warn "aftershipid not parsable as int"
              isJust' "afterlv" afterlv
              isJust' "afterfuel" afterfuel
              isJust' "afternull" afterbull

          case voicef of
            Just v -> unless (inRange (0, 7) v) $ warn $ "voicef not in range" <> show v
            Nothing -> warn "voicef is Nothing"
        else do
          isNothing' "sortno" sortno

          isNothing' "fuelMax" fuelMax
          isNothing' "bullMax" bullMax

          isNothing' "taik" taik
          isNothing' "souk" souk
          isNothing' "houg" houg
          isNothing' "raig" raig
          isNothing' "tyku" tyku

          isNothing' "luck" luck
          isNothing' "leng" leng

          isNothing' "maxeq" maxeq
          isNothing' "buildtime" buildtime
          isNothing' "broken" broken
          isNothing' "powup" powup
          isNothing' "backs" backs

          isNothing' "getmes" getmes
          isNothing' "afterlv" afterlv
          isNothing' "aftershipid" aftershipid
          isNothing' "afterfuel" afterfuel
          isNothing' "afternull" afterbull

          isNothing' "voicef" voicef

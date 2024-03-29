{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.Ship (
  Ship (..),
) where

import Data.Aeson as Aeson
import Data.Maybe
import qualified Data.Text as T
import Kantour.Core.KcData.Master.Direct.Common

data Ship = Ship
  { kcId :: Int
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

instance FromJSON Ship where
  parseJSON = parseKcMstJson

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
      { kcId
      , name
      , sortno
      , bullMax
      , fuelMax
      , taik
      , souk
      , houg
      , raig
      , tyku
      , tais
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
      } = fix \(_ :: m ()) -> do
      let warn :: String -> m ()
          warn msg = vLogS $ "Ship{" <> T.unpack name <> "," <> show kcId <> "}: " <> msg
          isJust', isNothing' :: (Show a, Eq a) => String -> Maybe a -> m ()
          isJust' tag var = unless (isJust var) do
            warn $ tag <> " should be Just"
          isStatShaped :: Show a => String -> Maybe [a] -> m ()
          isStatShaped tag var = case var of
            Just [_, _] -> pure ()
            _ -> warn $ tag <> " expect shape Just [_, _] but got: " <> show var
          expect :: (Show a, Eq a) => String -> a -> a -> m ()
          expect tag var e = unless (var == e) do
            warn $ tag <> " should be " <> show e <> ", got: " <> show var
          isNothing' tag var = expect tag var Nothing

      if kcId <= 1500
        then do
          isJust' "sortno" sortno
          isJust' "fuelMax" fuelMax
          isJust' "bullMax" bullMax

          isStatShaped "taik" taik
          isStatShaped "souk" souk
          isStatShaped "houg" houg
          isStatShaped "raig" raig
          isStatShaped "tyku" tyku
          case tais of
            Nothing -> pure ()
            Just [_] -> pure ()
            _ -> warn $ "unexpected shape: " <> show tais
          {-
            Reference: ShipMstModel > taisen_base
            "tais" could be nothing, in which case we fill in 0
           -}
          isStatShaped "luck" luck

          isJust' "maxeq" maxeq
          isJust' "buildtime" buildtime
          case broken of
            Just [_, _, _, _] -> pure ()
            _ -> warn $ "ill-formed broken: " <> show broken

          case powup of
            Just [_, _, _, _] -> pure ()
            _ -> warn $ "ill-formed broken: " <> show powup

          case backs of
            Nothing ->
              warn "backs should be Just"
            Just v -> unless (inRange (1, 8) v) do
              warn $ "backs should be in range [1,8]: " <> show v
          case afterlv of
            Nothing -> do
              isNothing' "aftershipid" aftershipid
              isNothing' "afterfuel" afterfuel
              isNothing' "afternull" afterbull
            Just _ -> do
              case aftershipid of
                Nothing -> warn "aftershipid should be Just"
                Just x -> unless (isIntParsable x) do
                  warn "aftershipid not parsable as int"
              isJust' "afterlv" afterlv
              isJust' "afterfuel" afterfuel
              isJust' "afternull" afterbull

          case voicef of
            Just v -> unless (inRange (0, 7) v) $ warn $ "voicef not in range: " <> show v
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
          isNothing' "tais" tais

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

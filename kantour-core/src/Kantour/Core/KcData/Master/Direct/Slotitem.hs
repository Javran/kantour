module Kantour.Core.KcData.Master.Direct.Slotitem (
  Slotitem (..),
) where

import Data.Aeson
import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Direct.Common

data Slotitem = Slotitem
  { slotId :: Int
  , sakb :: Int
  , bakk :: Int
  , tyku :: Int
  , leng :: Int
  , houg :: Int
  , version :: Maybe Int
  , tais :: Int
  , houm :: Int
  , saku :: Int
  , luck :: Int
  , raik :: Int
  , sortno :: Int
  , sType :: [Int]
  , raig :: Int
  , baku :: Int
  , souk :: Int
  , broken :: [Int]
  , taik :: Int
  , raim :: Int
  , name :: T.Text
  , usebull :: T.Text
  , atap :: Int
  , rare :: Int
  , houk :: Int
  , cost :: Maybe Int
  , distance :: Maybe Int
  , soku :: Int
  }
  deriving stock (Generic, Show)

instance FromJSON Slotitem where
  parseJSON = parseKcMstJson [("slotId", "id"), ("sType", "type")]

instance NFData Slotitem

instance HasKnownFields Slotitem where
  knownFields _ =
    kcFields
      "id sakb bakk tyku leng houg version tais houm saku luck raik \
      \sortno type raig baku souk broken taik raim name usebull atap \
      \rare houk cost distance soku"

instance Verifiable Slotitem where
  verify
    Slotitem
      { slotId
      , name
      , sType
      , taik
      , atap
      , raim = _
      , raik
      , bakk
      , sakb
      , luck
      , usebull
      , leng
      , rare
      , broken
      } = fix \(_ :: m ()) -> do
      let warn msg = vLogS $ "Slotitem{" <> T.unpack name <> "}: " <> msg
      when (length sType /= 5) do
        warn "sType supposed to have 5 elements"
        warn $ show slotId
      when (length broken /= 4) do
        warn "broken supposed to have 4 elements"
        warn $ show broken
      let expect :: (Show a, Eq a) => String -> a -> a -> m ()
          expect tag var e = when (var /= e) do
            warn $ tag <> " is not " <> show e <> ": " <> show var

      expect "taik" taik 0
      expect "atap" atap 0
      -- raim is not 0 for few items.
      -- expect "raim" raim 0
      expect "raik" raik 0
      expect "bakk" bakk 0
      expect "sakb" sakb 0
      expect "luck" luck 0
      expect "usebull" usebull "0"
      unless (inRange (0, 5) leng) do
        warn "leng not in range [0,5]"
        warn (show leng)
      unless (inRange (0, 7) rare) do
        warn "rare not in range [0,7]"
        warn (show rare)

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Direct.Ship (
  Ship (..),
) where

import Control.DeepSeq (NFData)
import Data.Aeson as Aeson
import qualified Data.Text as T
import Deriving.Aeson
import Kantour.Core.KcData.Master.Direct.Common

data Ship = Ship
  { shipId :: Int
  , ctype :: Int
  , name :: T.Text
  , slotNum :: Int
  , sortId :: Int
  , tyku :: Maybe [Int]
  , buildtime :: Maybe Int
  , leng :: Maybe Int
  , backs :: Maybe Int
  , voicef :: Maybe Int
  , houg :: Maybe [Int]
  , getmes :: Maybe T.Text
  , bullMax :: Maybe Int
  , fuelMax :: Maybe Int
  , luck :: Maybe [Int]
  , afterfuel :: Maybe Int
  , aftershipid :: Maybe T.Text
  , afterbull :: Maybe Int
  , sortno :: Maybe Int
  , yomi :: T.Text
  , souk :: Maybe [Int]
  , broken :: Maybe [Int]
  , taik :: Maybe [Int]
  , afterlv :: Maybe Int
  , soku :: Maybe Int
  , stype :: Maybe Int
  , maxeq :: Maybe [Int]
  , powup :: Maybe [Int]
  , raig :: Maybe [Int]
  , tais :: Maybe [Int]
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

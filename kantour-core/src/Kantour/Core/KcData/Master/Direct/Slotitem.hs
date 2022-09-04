{-# LANGUAGE TypeOperators #-}

module Kantour.Core.KcData.Master.Direct.Slotitem (
  Slotitem (..),
) where

import Control.DeepSeq (NFData)
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
  deriving
    (FromJSON)
    via CustomJSON
          '[ FieldLabelModifier
              ( Rename "slotId" "id" : Rename "sType" "type" : KcConvention
              )
           ]
          Slotitem

instance NFData Slotitem

instance HasKnownFields Slotitem where
  knownFields _ =
    kcFields
      "id sakb bakk tyku leng houg version tais houm saku luck raik \
      \sortno type raig baku souk broken taik raim name usebull atap \
      \rare houk cost distance soku"

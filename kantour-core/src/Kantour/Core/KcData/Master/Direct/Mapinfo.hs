{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Direct.Mapinfo (
  Mapinfo (..),
) where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Kantour.Core.KcData.Master.Direct.Common

data Mapinfo = Mapinfo
  { infotext :: T.Text
  , sallyFlag :: [Int]
  , requiredDefeatCount :: Maybe Int
  , kcId :: Int
  , item :: [Int]
  , maxMaphp :: Maybe Int
  , opetext :: T.Text
  , name :: T.Text
  , mapareaId :: Int
  , no :: Int
  , level :: Int
  }
  deriving stock (Generic, Show)

instance FromJSON Mapinfo where
  parseJSON = parseKcMstJson

instance NFData Mapinfo
instance HasKnownFields Mapinfo where
  knownFields _ =
    kcFields
      "infotext sally_flag required_defeat_count id item max_maphp \
      \opetext name maparea_id no level"

instance Verifiable Mapinfo where
  verify Mapinfo {kcId, mapareaId, no, sallyFlag, item} = do
    let warn msg = vLogS $ "Mapinfo{" <> show kcId <> "}: " <> msg
    when (kcId /= mapareaId * 10 + no) do
      warn "wrong mapareaId & no combination"
    case sallyFlag of
      [_, _, _] -> pure ()
      xs -> warn $ "sallyFlag expect exactly 3:" <> show xs
    case item of
      [_, _, _, _] -> pure ()
      xs -> warn $ "item expect exactly 4:" <> show xs

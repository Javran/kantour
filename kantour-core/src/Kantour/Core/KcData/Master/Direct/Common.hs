{-# LANGUAGE NoMonomorphismRestriction #-}

module Kantour.Core.KcData.Master.Direct.Common (
  CollectExtra (..),
  HasKnownFields (..),
  kcFields,
  Verifiable (..),
  vLogT,
  vLogS,
  module Control.Monad,
  NFData,
  parseKcMstJson,
  inRange,
  fix,
  Generic,
  findDuplicates,
) where

import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad.Writer
import Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types
import Data.Bifunctor
import qualified Data.DList as DL
import Data.Ix (inRange)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Proxy
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Generics

{-

  For a FromJSON instance, use CollectExtra to collect unknown fields.
  This works by having the type in question specify a set of known fields
  so that CollectExtra collects rest of it from the object.

  A datatype is considered incomplete if either of the following is true:
  - any of the fields are not yet supported.
  - no KnownFields instance.

  TODO: RejectUnknownFields doesn't seem to play well with deriving-aeson,
  I suspect this is due to all those string modification not beknown to
  aeson. we need to look into this.

  TODO: tests to make sure all fields are covered.

  TODO: there are some fixed-length lists that can be converted to tuples.

 -}
data CollectExtra a = CollectExtra
  { ceValue :: a
  , ceExtra :: [(T.Text, Value)]
  }

class HasKnownFields a where
  {-# MINIMAL knownFields | knownFieldsSet #-}
  knownFieldsSet :: forall p. p a -> S.Set T.Text
  knownFieldsSet p = S.fromList (knownFields p)

  knownFields :: forall p. p a -> [T.Text]
  knownFields p = S.toList (knownFieldsSet p)

kcFields :: T.Text -> [T.Text]
kcFields xs = ("api_" <>) <$> T.words xs

instance (FromJSON a, HasKnownFields a) => FromJSON (CollectExtra a) where
  parseJSON = withObject "CollectExtra" $ \obj -> do
    (ceValue :: a) <- parseJSON (Object obj)
    let ceExtra =
          filter ((`S.notMember` knownFieldsSet (Proxy :: Proxy a)) . fst)
            . (fmap . first) Data.Aeson.Key.toText
            . KM.toList
            $ obj
    pure $ CollectExtra {ceValue, ceExtra}

{-
  For verifying that a Direct object has certain properties.

  e.g.:

  - a field is of a specific shape
  - a field is a string but is always representing a number
  - for a list of things, there's an unique id field that we can use for indexing

  Note that this is meant for post-parse verification since
  we'll need to have a Direct object in the first place.

  The idea is that master data won't change very often so it doesn't make sense to
  verify those things whenever it's loaded. So instead we run some CLI tool from
  time to time to verify those assumptions are still holding, and let go of heavy
  checks whenever data is loaded.
 -}
class Verifiable a where
  verify :: MonadWriter (DL.DList T.Text) m => a -> m ()
  verify _ = pure ()

vLogT :: MonadWriter (DL.DList T.Text) m => T.Text -> m ()
vLogT = tell . DL.singleton

vLogS :: MonadWriter (DL.DList T.Text) m => String -> m ()
vLogS = vLogT . T.pack

parseKcMstJson ::
  (Generic a, GFromJSON Zero (Rep a)) =>
  Value ->
  Parser a
parseKcMstJson = genericParseJSON opt
  where
    renamer = \case
      "kcId" -> "id"
      "kcType" -> "type"
      x -> x
    opt =
      defaultOptions
        { fieldLabelModifier = ("api_" <>) . camelTo2 '_' . renamer
        }

findDuplicates :: Ord a => [a] -> [NE.NonEmpty a]
findDuplicates = mapMaybe f . NE.groupAllWith id
  where
    f x = do
      _ : _ <- pure $ NE.tail x
      pure x

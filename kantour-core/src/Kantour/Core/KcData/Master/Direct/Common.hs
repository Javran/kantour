module Kantour.Core.KcData.Master.Direct.Common (
  KcApiField,
  KcConvention,
  CollectExtra (..),
  HasKnownFields (..),
  kcFields,
) where

import Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor
import Data.Proxy
import qualified Data.Set as S
import qualified Data.Text as T
import Deriving.Aeson

type KcConvention = [CamelToSnake, KcApiField]

data KcApiField

instance StringModifier KcApiField where
  getStringModifier = ("api_" <>)

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

{-# LANGUAGE UndecidableInstances #-}

module Kantour.Core.KcData.Master.Org.Common (
  FromDirect (..),
  Side (..),
  NFData,
  Generic,
  illformed,
  textToInt,
  when,
  unless,
  throwError,
  intBoolFlag,
  AesonOrg (..),
) where

import Control.DeepSeq (NFData)
import Control.Monad.Except
import Control.Monad.Writer
import Data.Aeson
import qualified Data.DList as DL
import qualified Data.Text as T
import GHC.Generics (Generic)

class FromDirect a where
  type Source a

  fromDirect ::
    ( MonadWriter (DL.DList T.Text) m -- soft failure / warning
    , MonadError T.Text m -- hard failure
    ) =>
    Source a ->
    m a

-- A wrapper to allow direct conversion from JSON via fromDirect
newtype AesonOrg a = AesonOrg {getOrg :: a}

instance (FromJSON (Source a), FromDirect a) => FromJSON (AesonOrg a) where
  parseJSON src = do
    directVal <- parseJSON @(Source a) src
    case runExcept $ runWriterT $ fromDirect directVal of
      Left msg -> fail (T.unpack msg)
      Right (v, _) -> pure (AesonOrg v)

{-
  (Currently unused)

  This is just in case we want to have some distinctions at type-level:

  For Ships:
  - x <= 1500 is Our
  - x > 1500 is Abyssal

  For Ship Graphs:
  - x <= 1500 is Our
  - 1500 < x < 5000 is Abyssal
  - x >= 5000 is Seasonal

  For Equips:
  - x <= 500 is Our
  - x > 500 is Abyssal

 -}
data Side = Our | Abyssal | Seasonal

illformed :: MonadError T.Text m => T.Text -> m a
illformed what = throwError $ "Ill-formed `" <> what <> "`"

textToInt :: T.Text -> Maybe Int
textToInt t = do
  [(v, "")] <- pure $ reads (T.unpack t)
  pure v

intBoolFlag :: (Eq a, Num a, MonadError T.Text f) => T.Text -> a -> f Bool
intBoolFlag tag = \case
  0 -> pure False
  1 -> pure True
  _ -> illformed tag

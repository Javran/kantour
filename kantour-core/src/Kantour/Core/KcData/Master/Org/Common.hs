module Kantour.Core.KcData.Master.Org.Common (
  FromDirect (..),
  Side (..),
  NFData,
  Generic,
  illformed,
) where

import Control.DeepSeq (NFData)
import Control.Monad.Except
import Control.Monad.Writer
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

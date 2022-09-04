module Kantour.Core.KcData.Master.Org.Common (
  FromDirect (..),
  NFData,
  Generic,
  illformed
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

illformed :: MonadError T.Text m => T.Text -> m a
illformed what = throwError $ "Ill-formed `" <> what <> "`"

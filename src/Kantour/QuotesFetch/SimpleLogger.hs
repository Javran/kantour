{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kantour.QuotesFetch.SimpleLogger where

import Control.Monad.Logger
import Control.Monad.State
import Control.Arrow
import System.Log.FastLogger
import qualified Data.Text as T
import Data.Text.Encoding

data LogMessage = LogMessage Loc LogSource LogLevel LogStr

newtype SimpleLogger a =
  SimpleLogger (State [LogMessage] a)
  deriving (Functor, Applicative, Monad)

instance MonadLogger SimpleLogger where
    monadLoggerLog loc ls ll msg =
        SimpleLogger $ modify (LogMessage loc ls ll (toLogStr msg):)

runSimpleLogger :: SimpleLogger a -> (a, [LogMessage])
runSimpleLogger (SimpleLogger m) =
    second reverse $ runState m []

runSimpleLoggerWithFilter ::
    SimpleLogger a ->
    (LogMessage -> Bool) ->
    (a, [LogMessage])
runSimpleLoggerWithFilter m f =
    second (filter f) (runSimpleLogger m)

logMessageToStr :: LogMessage -> String
logMessageToStr (LogMessage lc ls ll content) =
      T.unpack
    . decodeUtf8
    . fromLogStr
    $ logStrContent
  where
    logStrContent = defaultLogStr lc ls ll content

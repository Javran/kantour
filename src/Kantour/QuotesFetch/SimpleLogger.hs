{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kantour.QuotesFetch.SimpleLogger where

import Control.Monad.Logger
import Control.Monad.State
import Control.Arrow

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

-- TODO: try use this in place of stdout logger

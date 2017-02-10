{-# LANGUAGE TemplateHaskell #-}
module Types
  ( MyLine(..)
  , lName, lStart, lEnd
  ) where

import Linear
import Control.Lens

data MyLine = MyLine
  { _lName :: String
  , _lStart :: Maybe (V2 Int)
  , _lEnd :: V2 Int
  } deriving Show

makeLenses ''MyLine

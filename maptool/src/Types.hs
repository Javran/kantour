{-# LANGUAGE TemplateHaskell #-}
module Types
  ( MyLine(..)
  , lName, lStart, lEnd
  , simpleLName
  ) where

import Linear
import Control.Lens

data MyLine = MyLine
  { _lName :: String
  , _lStart :: V2 Int
  , _lEnd :: V2 Int
  } deriving Show

makeLenses ''MyLine

simpleLName :: MyLine -> String
simpleLName l = case splitAt 4 n of
    ("line",sName) -> sName
    _ -> n
  where
    n = _lName l

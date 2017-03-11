{-# LANGUAGE TemplateHaskell #-}
module Kantour.MapTool.Types
  ( MyLine(..)
  , lName, lStart, lEnd
  , simpleLName

  , MapInfo(..)
  , getNodeName
  , miLines, miNodeNames, miStarts -- TODO: this might be overkill...

  , ShapeBounds
  ) where

import Linear
import Control.Lens
import qualified Data.Set as S
import qualified Data.Map.Strict as M

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

data MapInfo = MapInfo
  { _miLines :: [MyLine]
  , _miStarts :: S.Set (V2 Int)
  , _miNodeNames :: M.Map (V2 Int) String -- ^ INVARIANT: all nodes have a name in it
  }

makeLenses ''MapInfo

getNodeName :: V2 Int -> MapInfo -> String
getNodeName p MapInfo { _miNodeNames=mn }
    | Just n <- M.lookup p mn = n
    | otherwise = error $ "missing node name for " ++ show p

type ShapeBounds =
    ( (Int, Int) -- x max, min
    , (Int, Int) -- y max, min
    )

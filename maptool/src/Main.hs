{-# OPTIONS_GHC
    -fno-warn-partial-type-signatures
  #-}
{-# LANGUAGE
    Arrows
  , PartialTypeSignatures
  , ScopedTypeVariables
  , NoMonomorphismRestriction
  #-}
module Main where

import System.Environment
import Data.List
import Data.Maybe
import Text.XML.HXT.Core hiding (when)
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Linear
import Control.Lens hiding (deep)
import Linear.Affine
import Data.Function
import Control.Monad

import Types
import Draw
{-

implementation of:

https://github.com/yukixz/kcmap/blob/master/kcmap.es

in Haskell

and perhaps more.

related links:

an explanation can be found at:

http://blog.dazzyd.org/blog/how-to-draw-a-kancolle-map/

-}

getIntPair :: ArrowXml arr => String -> String -> (Int -> Int -> a) -> arr XmlTree a
getIntPair fstName sndName resultF =
    (   (getAttrValue fstName >>> asInt)
    &&& (getAttrValue sndName >>> asInt)
    ) >>> arr (uncurry resultF)
  where
    asInt = arr (read :: String -> Int)

getRoute :: IOSArrow XmlTree _
getRoute = proc doc -> do
    mapId <- deep (hasName "item" >>>
                    hasAttrValue "name" (== "map") >>>
                    getAttrValue "characterId") -< doc
    lineRefs <- deep (hasName "item" >>>
                      hasAttrValue "spriteId" (== mapId)) />
               hasName "subTags" /> hasName "item" -<< doc
    -- each of these item corresponds to a line (route)
    line <- hasAttrValue "name" ("line" `isPrefixOf`) -<< lineRefs

    lineName <- getAttrValue "name" -< line
    mat <- this /> hasName "matrix" -< line
    -- end point coordinate (or the origin of the "item" resource)
    ptEnd <- getIntPair "translateX" "translateY" V2 -< mat

    -- gettling line sprite
    spriteId <- getAttrValue "characterId" -< lineRefs
    sprite <- deep (hasName "item" >>>
                    hasAttrValue "spriteId" (== spriteId)) -<< doc
    -- shape should be a child of sprite (the line)
    shapeRef <- this /> hasName "subTags"
                     /> hasAttr "characterId" -< sprite

    shapeId <- getAttrValue "characterId" -< shapeRef
    matSp <- this /> hasName "matrix" -< shapeRef

    sh <- getIntPair "translateX" "translateY" V2 >>>
          arrIO (\v -> when (v /= V2 0 0)
                       (putStrLn $ "Warning .. non-zero shape origin: " ++ show v) >> pure v)
          -<< matSp
    shape <- deep (hasName "item" >>>
                   hasAttrValue "shapeId" (== shapeId) />
                   hasName "shapeBounds")
             -<< doc
    (xMax,xMin) <- getIntPair "Xmax" "Xmin" (,) -< shape
    (yMax,yMin) <- getIntPair "Ymax" "Ymin" (,) -< shape
    let dx = (view _x sh + ((xMax + xMin) `div` 2)) * 2
    let dy = (view _y sh + ((yMax + yMin) `div` 2)) * 2
        mPtStart = if dx == 0 && dy == 0
                     then Nothing
                     else Just (ptEnd + V2 dx dy)
    this -< MyLine lineName mPtStart ptEnd

getMapBeginNode :: IOSArrow XmlTree _
getMapBeginNode = proc doc -> do
    mapId <- deep (hasName "item" >>>
                    hasAttrValue "name" (== "map") >>>
                    getAttrValue "characterId") -< doc
    lineRefs <- deep (hasName "item" >>>
                      hasAttrValue "spriteId" (== mapId)) />
               hasName "subTags" /> hasName "item" -<< doc
    -- each of these item corresponds to a line (route)
    line <- hasAttrValue "name" ("line" `isPrefixOf`) -<< lineRefs
    mat <- this /> hasName "matrix" -< line
    -- end point coordinate (or the origin of the "item" resource)
    ptEnd <- getIntPair "translateX" "translateY" V2 -< mat
    -- gettling line sprite
    spriteId <- getAttrValue "characterId" -< lineRefs
    sprite <- deep (hasName "item" >>>
                    hasAttrValue "spriteId" (== spriteId)) -<< doc
    -- shape should be a child of sprite (the line)
    (this /> hasName "subTags") `notContaining` (this /> hasAttr "characterId")
        -< sprite
    this -< ptEnd

main :: IO ()
main = do
    srcFP : remained <- getArgs
    mDoc <- runX (readDocument [] srcFP)
    let doc = fromMaybe (error "source document parsing error") $ listToMaybe mDoc
    results <- runWithDoc_ getRoute doc
    beginNodes <- runWithDoc_ getMapBeginNode doc
    putStrLn "====="
    mapM_ print results
    -- the coordinates look like large numbers because SWF uses twip as basic unit
    -- (most of the time) divide them by 20 to get pixels
    let adjusted = adjustLines beginNodes results
    withArgs remained $ draw adjusted

runWithDoc_ :: IOSLA _ XmlTree a -> XmlTree -> IO [a]
runWithDoc_ (IOSLA f) doc = snd <$> f (initialState ()) doc

adjustLines :: [V2 Int] -> [MyLine] -> [MyLine]
adjustLines startPts ls = adjustLine <$> ls
  where
    confirmedPoints = startPts ++ (_lEnd <$> ls)
    adjustLine :: MyLine -> MyLine
    adjustLine l@(MyLine _ Nothing _) = l
    adjustLine l@(MyLine _ (Just lStartPt) _) = l { _lStart = Just adjustedStartPt }
      where
        adjustedStartPt = minimumBy (compare `on` qdA lStartPt) confirmedPoints

{-# OPTIONS_GHC
    -fno-warn-partial-type-signatures
  #-}
{-# LANGUAGE
    Arrows
  , PartialTypeSignatures
  #-}
module Main where

import System.Environment
import Data.List
import Text.XML.HXT.Core
import Data.Maybe
import Linear
import Control.Lens hiding (deep)
import Linear.Affine
import Data.Function

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

getRange :: [MyLine] -> ((Int,Int),(Int,Int))
getRange xs = ((minimum xCoords, maximum xCoords),(minimum yCoords,maximum yCoords))
  where
    -- xCoords :: _
    xCoords = concatMap (\(MyLine _ p1 p2) -> maybeToList (view _x <$> p1) ++ [view _x p2]) xs
    yCoords = concatMap (\(MyLine _ p1 p2) -> maybeToList (view _y <$> p1) ++ [view _y p2]) xs

getRoute :: IOSArrow XmlTree _
getRoute = proc doc -> do
    rawCId <- deep (hasName "item" >>>
                    hasAttrValue "name" (== "map") >>>
                    getAttrValue "characterId") -< doc
    lineRefs <- deep (hasName "item" >>>
                     hasAttrValue "spriteId" (== rawCId)) />
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
                     /> hasAttrValue "characterId" (pure True) -< sprite

    shapeId <- getAttrValue "characterId" -< shapeRef
    matSp <- this /> hasName "matrix" -< shapeRef
    -- (TODO) actually it's suppose to be the case that "(shX,shY) + endPoint" is the real node point..
    -- but let's not worry about that right now.
    (shX,shY) <- getIntPair "translateX" "translateY" (,) -< matSp
    shape <- deep (hasName "item" >>>
                   hasAttrValue "shapeId" (== shapeId) />
                   hasName "shapeBounds")
             -<< doc
    (xMax,xMin) <- getIntPair "Xmax" "Xmin" (,) -< shape
    (yMax,yMin) <- getIntPair "Ymax" "Ymin" (,) -< shape
    let dx = (shX + ((xMax + xMin) `div` 2)) * 2
    let dy = (shY + ((yMax + yMin) `div` 2)) * 2
        mPtStart = if dx == 0 && dy == 0
                     then Nothing
                     else Just (V2 (ptEnd^._x+dx) (ptEnd^._y+dy))
    startMat <- deep (hasAttrValue "name" (== "line0")) /> hasName "matrix" -< doc
    ptMapStart <- getIntPair "translateX" "translateY" V2 -< startMat
    -- I guess probably we have to live with the fact that ptMapStart has to be carried
    -- around duplicated..
    this -< (MyLine lineName mPtStart ptEnd, ptMapStart)
  where
    asInt = arr (read :: String -> Int)
    getIntPair :: String -> String -> (Int -> Int -> a) -> _ _ a
    getIntPair fstName sndName resultF =
        (   (getAttrValue fstName >>> asInt)
        &&& (getAttrValue sndName >>> asInt)
        ) >>> arr (uncurry resultF)

main :: IO ()
main = do
    srcFP : remained <- getArgs
    -- [srcFP] <- getArgs
    results <- runX (readDocument [] srcFP >>> getRoute)
    mapM_ print results
    -- the coordinates look like large numbers because SWF uses twip as basic unit
    -- (most of the time) divide them by 20 to get pixels
    -- print (getRange results)
    let startPoint = snd (head results)
        adjusted = adjustLines startPoint (fst <$> results)
    withArgs remained $ draw startPoint adjusted

adjustLines :: V2 Int -> [MyLine] -> [MyLine]
adjustLines startPt ls = adjustLine <$> ls
  where
    confirmedPoints = startPt : (_lEnd <$> ls)
    adjustLine :: MyLine -> MyLine
    adjustLine l@(MyLine _ Nothing _) = l
    adjustLine l@(MyLine _ (Just lStartPt) _) = l { _lStart = Just adjustedStartPt }
      where
        adjustedStartPt = minimumBy (compare `on` qdA lStartPt) confirmedPoints

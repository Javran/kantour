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
import Control.Monad

{-

implementation of:

https://github.com/yukixz/kcmap/blob/master/kcmap.es

in Haskell

and perhaps more.

-}

data Point = Point
  { ptX :: Int
  , ptY :: Int
  } deriving Show

data Line = Line
  { lName :: String
  , lStart :: Maybe Point
  , lEnd :: Point
  } deriving Show

getRange :: [Line] -> ((Int,Int),(Int,Int))
getRange xs = ((minimum xCoords, maximum xCoords),(minimum yCoords,maximum yCoords))
  where
    xCoords = concatMap (\(Line _ p1 p2) -> maybeToList (ptX <$> p1) ++ [ptX p2]) xs :: [Int]
    yCoords = concatMap (\(Line _ p1 p2) -> maybeToList (ptY <$> p1) ++ [ptY p2]) xs :: [Int]

getRoute :: IOSArrow XmlTree Line
getRoute = proc doc -> do
    rawCId <- deep (hasName "item" >>>
                    hasAttrValue "name" (== "map") >>>
                    getAttrValue "characterId") -< doc
    lineRef <- deep (hasName "item" >>>
                     hasAttrValue "spriteId" (== rawCId)) />
               hasName "subTags" />
               hasName "item" -<< doc
    line <- hasAttrValue "name" ("line" `isPrefixOf`) -<< lineRef
    lineName <- getAttrValue "name" -< line
    mat <- this /> hasName "matrix" -< line
    ptEnd <- getIntPair "translateX" "translateY" Point -< mat
    spriteId <- getAttrValue "characterId" -< lineRef
    sprite <- deep (hasName "item" >>>
                    hasAttrValue "spriteId" (== spriteId)) -<< doc
    shapeRef <- this /> hasName "subTags"
                     /> hasAttrValue "characterId" (pure True) -< sprite
    shapeId <- getAttrValue "characterId" -< shapeRef
    matSp <- this /> hasName "matrix" -< shapeRef
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
                     else Just (Point (ptX ptEnd+dx) (ptY ptEnd+dy))
    this -< Line lineName mPtStart ptEnd
  where
    asInt = arr (read :: String -> Int)
    getIntPair :: String -> String -> (Int -> Int -> a) -> _ _ a
    getIntPair fstName sndName resultF =
        (   (getAttrValue fstName >>> asInt)
        &&& (getAttrValue sndName >>> asInt)
        ) >>> arr (uncurry resultF)

main :: IO ()
main = do
    [srcFP] <- getArgs
    results <- runX (readDocument [] srcFP >>> getRoute)
    mapM_ print results
    print (getRange results)

    {-
    -- for gnuplot
    forM_ results $ \(Line _ (Just ptStart) ptEnd) -> do
        putStrLn $ show (ptX ptStart) ++ " " ++ show (ptY ptStart)
        putStrLn $ show (ptX ptEnd) ++ " " ++ show (ptY ptEnd)
        putStrLn "" -}


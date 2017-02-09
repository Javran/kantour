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

data Point = Point Int Int
  deriving Show

data Line = Line
  { lName :: String
  , lStart :: Maybe Point
  , lEnd :: Point
  } deriving Show

getX, getY :: Point -> Int

getX (Point x _) = x
getY (Point _ y) = y

getRange :: [Line] -> ((Int,Int),(Int,Int))
getRange xs = ((minimum xCoords, maximum xCoords),(minimum yCoords,maximum yCoords))
  where
    xCoords = concatMap (\(Line _ p1 p2) -> maybeToList (getX <$> p1) ++ [getX p2]) xs :: [Int]
    yCoords = concatMap (\(Line _ p1 p2) -> maybeToList (getY <$> p1) ++ [getY p2]) xs :: [Int]

main :: IO ()
main = do
    [srcFP] <- getArgs
    results <- runX (readDocument [] srcFP >>> proc doc -> do
                         rawCId <- deep (hasName "item" >>>
                                         hasAttrValue "name" (== "map") >>>
                                         getAttrValue "characterId") -< doc
                         lineRef <- deep (hasName "item" >>>
                                          hasAttrValue "spriteId" (== rawCId)) />
                                    hasName "subTags" />
                                    hasName "item" -<< doc
                         line <- hasAttrValue "name" ("line" `isPrefixOf`) -<< lineRef
                         lineName <- getAttrValue "name" -< line
                         mat <- returnA /> hasName "matrix" -< line
                         x <- getAttrValue "translateX" >>> asInt -< mat
                         y <- getAttrValue "translateY" >>> asInt  -< mat
                         let ptEnd = Point x y
                         spriteId <- getAttrValue "characterId" -< lineRef
                         sprite <- deep (hasName "item" >>>
                                         hasAttrValue "spriteId" (== spriteId)) -<< doc
                         shapeRef <- returnA /> hasName "subTags"
                                     /> hasAttrValue "characterId" (pure True) -< sprite
                         shapeId <- getAttrValue "characterId" -< shapeRef
                         matSp <- returnA /> hasName "matrix" -< shapeRef
                         shX <- getAttrValue "translateX" >>> asInt -< matSp
                         shY <- getAttrValue "translateY" >>> asInt -< matSp

                         shape <- deep (hasName "item" >>>
                                        hasAttrValue "shapeId" (== shapeId) />
                                        hasName "shapeBounds")
                                  -<< doc
                         xMax <- getAttrValue "Xmax" >>> asInt -< shape
                         xMin <- getAttrValue "Xmin" >>> asInt -< shape

                         yMax <- getAttrValue "Ymax" >>> asInt -< shape
                         yMin <- getAttrValue "Ymin" >>> asInt -< shape
                         let dx = (shX + ((xMax + xMin) `div` 2)) * 2
                         let dy = (shY + ((yMax + yMin) `div` 2)) * 2
                             mPtStart = if dx == 0 && dy == 0
                                          then Nothing
                                          else Just (Point (x+dx) (y+dy))
                         returnA -< Line lineName mPtStart ptEnd
                    )
    mapM_ print results
    print (getRange results)

    {-
    -- for gnuplot
    forM_ results $ \(Line _ (Just ptStart) ptEnd) -> do
        putStrLn $ show (getX ptStart) ++ " " ++ show (getY ptStart)
        putStrLn $ show (getX ptEnd) ++ " " ++ show (getY ptEnd)
        putStrLn "" -}
  where
    asInt = arr (read :: String -> Int)

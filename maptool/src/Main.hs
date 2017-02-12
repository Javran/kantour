{-# OPTIONS_GHC
    -fwarn-partial-type-signatures
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
import Text.JSON
import qualified Data.Map.Strict as M

import Types
import Draw

type ShapeBounds = ( (Int, Int) -- x min to max
                   , (Int, Int) -- y min to max
                   )
{-

implementation of:

https://github.com/yukixz/kcmap/blob/master/kcmap.es

in Haskell

and perhaps more.

related links:

an explanation can be found at:

http://blog.dazzyd.org/blog/how-to-draw-a-kancolle-map/

-}

{-
TODO: http://stackoverflow.com/a/13472431
-}

getIntPair :: ArrowXml arr => String -> String -> (Int -> Int -> a) -> arr XmlTree a
getIntPair fstName sndName resultF =
    (   (getAttrValue fstName >>> asInt)
    &&& (getAttrValue sndName >>> asInt)
    ) >>> arr (uncurry resultF)
  where
    asInt = arr (read :: String -> Int)

loadSubMatrix :: ArrowXml arr => arr XmlTree (V2 Int)
loadSubMatrix = this
    /> hasName "matrix"
    >>> getIntPair "translateX" "translateY" V2

findShapeBounds :: ArrowXml arr => String -> arr XmlTree ShapeBounds
findShapeBounds shapeId = proc doc -> do
    shape <- deep (hasName "item" >>>
                   hasAttrValue "shapeId" (== shapeId) />
                   hasName "shapeBounds")
             -<< doc
    xMaxMin <- getIntPair "Xmax" "Xmin" (,) -< shape
    yMaxMin <- getIntPair "Ymax" "Ymin" (,) -< shape
    this -< (xMaxMin, yMaxMin)

guessStartPoint :: V2 Int -> V2 Int -> ShapeBounds -> Maybe (V2 Int)
guessStartPoint ptEnd sh ((xMax,xMin),(yMax,yMin)) = if dx == 0 && dy == 0
    then Nothing
    else Just (ptEnd + V2 dx dy)
  where
    dx = (view _x sh + ((xMax + xMin) `div` 2)) * 2
    dy = (view _y sh + ((yMax + yMin) `div` 2)) * 2

findMapSprite :: ArrowXml arr => arr XmlTree XmlTree
findMapSprite = proc doc -> do
    mapId <- deep
        (hasName "item"
         >>> hasAttrValue "name" (== "map")
         >>> getAttrValue "characterId") -< doc
    deep
        (hasName "item"
         >>> hasAttrValue "spriteId" (== mapId)) -<< doc

findLineShapeInfo :: ArrowXml arr => String -> arr XmlTree (ShapeBounds,V2 Int)
findLineShapeInfo lineId = proc doc -> do
    sprite <- deep (hasName "item" >>>
                    hasAttrValue "spriteId" (== lineId)) -<< doc
    -- shape should be a child of sprite (the line)
    shapeRef <- this /> hasName "subTags"
                     /> hasAttr "characterId" -< sprite
    shapeId <- getAttrValue "characterId" -< shapeRef
    sh <- loadSubMatrix -< shapeRef
    sb <- findShapeBounds shapeId -<< doc
    this -< (sb,sh)

getRoute :: IOSArrow XmlTree MyLine
getRoute = proc doc -> do
    mapSprite <- findMapSprite -< doc
    lineRef <- this
         /> hasName "subTags"
         /> hasName "item"
         >>> hasAttrValue "name" ("line" `isPrefixOf`) -<< mapSprite
    lineName <- getAttrValue "name" -< lineRef
    -- end point coordinate (or the origin of the "item" resource)
    ptEnd <- loadSubMatrix -< lineRef
    lineId <- getAttrValue "characterId" -< lineRef
    (sb,sh) <- findLineShapeInfo lineId -<< doc
    this -< MyLine lineName (guessStartPoint ptEnd sh sb) ptEnd

getExtraRoute :: IOSArrow XmlTree MyLine
getExtraRoute = proc doc -> do
    mapSprite <- findMapSprite -< doc
    mapRefs <- this />
               hasName "subTags" /> hasName "item" -< mapSprite
    extra <- hasAttrValue "name" ("extra" `isPrefixOf`) -<< mapRefs
    -- extra origin extra
    ptExEnd <- loadSubMatrix -< extra

    extraId <- getAttrValue "characterId" -< extra
    sprite' <- deep (hasName "item" >>>
                     hasAttrValue "spriteId" (== extraId)) -<< doc
    this -< sprite'
    -- each of these item corresponds to a line (route)
    lineRef <- this /> hasName "subTags" /> hasAttrValue "name" ("line" `isPrefixOf`) -<< sprite'

    lineName <- getAttrValue "name" -< lineRef
    ptEnd' <- loadSubMatrix -< lineRef
    -- end point coordinate (or the origin of the "item" resource)
    let ptEnd = ptEnd' ^+^ ptExEnd
    -- gettling line sprite
    spriteId <- getAttrValue "characterId" -< lineRef
    (sb,sh) <- findLineShapeInfo spriteId -<< doc
    this -< MyLine lineName (guessStartPoint ptEnd sh sb) ptEnd

getMapBeginNode :: IOSArrow XmlTree (V2 Int)
getMapBeginNode = proc doc -> do
    mapId <- deep (hasName "item" >>>
                    hasAttrValue "name" (== "map") >>>
                    getAttrValue "characterId") -< doc
    lineRefs <- deep (hasName "item" >>>
                      hasAttrValue "spriteId" (== mapId)) />
               hasName "subTags" /> hasName "item" -<< doc
    -- each of these item corresponds to a line (route)
    line <- hasAttrValue "name" ("line" `isPrefixOf`) -<< lineRefs
    ptEnd <- loadSubMatrix -< line
    -- getting line sprite
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
    results2 <- runWithDoc_ getExtraRoute doc
    beginNodes <- runWithDoc_ getMapBeginNode doc
    putStrLn "====="
    -- the coordinates look like large numbers because SWF uses twip as basic unit
    -- (most of the time) divide them by 20 to get pixels
    let adjusted = adjustLines beginNodes (results ++ results2)
        pointMap = mkPointMap beginNodes adjusted
    withArgs remained $ draw adjusted pointMap
    putStrLn "=== JSON encoding ==="
    putStrLn (encodeStrict (linesToJSValue adjusted pointMap))
    pure ()

runWithDoc_ :: IOSLA (XIOState ()) XmlTree a -> XmlTree -> IO [a]
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

mkPointMap :: [V2 Int] -> [MyLine] -> M.Map (V2 Int) String
mkPointMap beginNodes xs =
    M.union beginNodeMaps (M.map getMin
                           (M.fromListWith (++)
                            (map convert xs)))
  where
    getMin = minimumBy (\x y -> case compare (length x) (length y) of
                            EQ -> compare x y
                            v -> v)
    beginNodeMaps = M.fromList (zip beginNodes ((\x -> "<" ++ show x ++ ">") <$> [1::Int ..]))
    lineToInt l = read (drop 4 $ _lName l)
    nodeNameFromInt v
        | v-1< length ns = ns !! (v-1)
        | otherwise = show v
      where
        ns = map (:[]) ['A'..'Z']

    convert l = (_lEnd l,[nodeNameFromInt .lineToInt $ l])

linesToJSValue :: [MyLine] -> M.Map (V2 Int) String -> JSValue
linesToJSValue xs nnames = JSObject (toJSObject (convert <$> ys))
  where
    ys = sortBy (compare `on` (\l -> read (drop 4 (_lName l)) :: Int)) xs
    getNm v = makeStart (fromMaybe "Unknown" (M.lookup v nnames))
      where
        makeStart ('<':_) = "Start"
        makeStart v' = v'
    convert :: MyLine -> (String, JSValue)
    convert l = (drop 4 (_lName l),JSArray (f <$> [getNm (fromJust $ _lStart l),getNm (_lEnd l)]))
      where
        f = JSString . toJSString

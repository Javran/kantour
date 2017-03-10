{-# OPTIONS_GHC
    -fwarn-partial-type-signatures
  #-}
{-# LANGUAGE
    Arrows
  , PartialTypeSignatures
  , ScopedTypeVariables
  , NoMonomorphismRestriction
  #-}
module Kantour.MapTool.Main where

import System.Environment
import Data.List
import Data.Maybe
import Control.Monad
import Text.XML.HXT.Core hiding (when)
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Linear
import Control.Lens hiding (deep)
import Linear.Affine
import Data.Function
import Text.JSON
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Exit

import Kantour.MapTool.Types
import Kantour.MapTool.Draw

type ShapeBounds =
    ( (Int, Int) -- x max, min
    , (Int, Int) -- y max, min
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

getIntPair :: ArrowXml arr => String -> String -> (Int -> Int -> a) -> arr XmlTree a
getIntPair fstName sndName resultF =
    (   (getAttrValue fstName >>> asInt)
    &&& (getAttrValue sndName >>> asInt)
    ) >>> arr (uncurry resultF)
  where
    asInt = arr (read :: String -> Int)

loadSubMatrixAndId :: ArrowXml arr => arr XmlTree (V2 Int, String)
loadSubMatrixAndId = proc tree -> do
    pt <- loadSubMatrix -< tree
    charId <- getAttrValue "characterId" -< tree
    this -< (pt,charId)
  where
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

guessStartPoint :: V2 Int -> V2 Int -> ShapeBounds -> V2 Int
guessStartPoint ptEnd sh ((xMax,xMin),(yMax,yMin)) = ptEnd + V2 dx dy
  where
    dx = (view _x sh + ((xMax + xMin) `div` 2)) * 2
    dy = (view _y sh + ((yMax + yMin) `div` 2)) * 2

findSprite :: ArrowXml arr => String -> arr XmlTree XmlTree
findSprite spriteId =
    deep (hasName "item" >>>
          hasAttrValue "spriteId" (== spriteId))

findMapSprite :: ArrowXml arr => arr XmlTree XmlTree
findMapSprite = proc doc -> do
    mapId <- deep
        (hasName "item"
         >>> hasAttrValue "name" (== "map")
         >>> getAttrValue "characterId") -< doc
    findSprite mapId -<< doc

findLineShapeInfo :: ArrowXml arr => String -> arr XmlTree (ShapeBounds,V2 Int)
findLineShapeInfo lineId = proc doc -> do
    sprite <- findSprite lineId -<< doc
    -- shape should be a child of sprite (the line)
    shapeRef <- getChild >>> hasAttr "characterId" -< sprite
    (sh,shapeId) <- loadSubMatrixAndId -< shapeRef
    sb <- findShapeBounds shapeId -<< doc
    this -< (sb,sh)

getChild :: ArrowXml arr => arr XmlTree XmlTree
getChild = this /> hasName "subTags" /> hasName "item"

getRoute :: IOSArrow XmlTree MyLine
getRoute = proc doc -> do
    (lineName,(ptEnd,lineId)) <-
        findMapSprite
        >>> getChild
        >>> hasAttrValue "name" ("line" `isPrefixOf`)
        >>> getAttrValue "name" &&& loadSubMatrixAndId -< doc
    (sb,sh) <- findLineShapeInfo lineId -<< doc
    this -< MyLine lineName (guessStartPoint ptEnd sh sb) ptEnd

getExtraRoute :: IOSArrow XmlTree MyLine
getExtraRoute = proc doc -> do
    (ptExEnd,extraId) <-
        findMapSprite
        >>> getChild
        >>> loadSubMatrixAndId -< doc
    (lineName, (ptEnd',spriteId)) <-
        findSprite extraId
        >>> getChild
        >>> hasAttrValue "name" ("line" `isPrefixOf`)
        >>> getAttrValue "name" &&& loadSubMatrixAndId -<< doc
    let ptEnd = ptEnd' ^+^ ptExEnd
    (sb,sh) <- findLineShapeInfo spriteId -<< doc
    this -< MyLine lineName (guessStartPoint ptEnd sh sb) ptEnd

getMapBeginNode :: IOSArrow XmlTree (V2 Int)
getMapBeginNode = proc doc -> do
    -- load line info
    (ptEnd,spriteId) <-
        findMapSprite
        >>> getChild
        >>> hasAttrValue "name" ("line" `isPrefixOf`)
        >>> loadSubMatrixAndId -< doc
    -- select lines whose childrens are without ids
    findSprite spriteId
        >>> ((this /> hasName "subTags")
             `notContaining`
             (this /> hasAttr "characterId")) -<< doc
    this -< ptEnd

defaultMain :: IO ()
defaultMain = do
    mArgs <- sepArgs <$> getArgs
    case mArgs of
        Nothing -> do
            putStrLn "invalid arguments"
            putStrLn "usage: maptool <main xml> [extra xml] [-- diagrams args]"
            putStrLn "the argument list passing to diagrams, if exists, has to be non empty"
            exitFailure
        Just ((srcFP, mExtraFP), mDiagramArgs) -> do
            -- pretty printing arguments
            putStrLn $ "main xml: " ++ srcFP
            putStrLn $ "extra xml: " ++ fromMaybe "<N/A>" mExtraFP
            putStrLn $ "args to diagrams: " ++ maybe "<N/A>" unwords mDiagramArgs

            mDoc <- runX (readDocument [] srcFP)
            let doc = fromMaybe (error "source document parsing error") $ listToMaybe mDoc
            [((results,results2),beginNodes)] <- runWithDoc_
                ((listA getRoute &&& listA getExtraRoute) &&& listA getMapBeginNode)
                doc
            putStrLn "====="
            -- the coordinates look like large numbers because SWF uses twip as basic unit
            -- (most of the time) divide them by 20 to get pixels
            let adjusted = adjustLines beginNodes (results ++ results2)
                pointMap = mkPointMap beginNodes adjusted
                mapInfo = MapInfo adjusted (S.fromList beginNodes) pointMap
            case mDiagramArgs of
                Nothing -> pure ()
                Just diagramArgs ->
                    withArgs diagramArgs $ draw mapInfo
            putStrLn "=== JSON encoding ==="
            putStrLn (encodeStrict (linesToJSValue adjusted pointMap))

-- separate argument list into maptool arguments and those meant for diagrams:
-- arg list: <main xml> [extra xml] [-- <diagram args>]
-- where <main xml> is the map xml file, [extra xml] is an optional part.
-- additionally, if "--" exists and <diagram args> is not empty, diagram will be called
-- to draw a picture.
sepArgs :: [String] -> Maybe ((String, Maybe String), Maybe [String])
sepArgs as = do
    let (ls,rs') = break (== "--") as
    lVal <- case ls of
        [] -> Nothing
        mainXmlFP : ls' -> case ls' of
            [] -> pure (mainXmlFP, Nothing)
            [extraXmlFP] -> pure (mainXmlFP, Just extraXmlFP)
            _ -> Nothing
    let rVal = case rs' of
            [] -> Nothing
            -- the "_" part as to be "--" as it's the result from "break"
            _:xs -> guard (not (null xs)) >> pure xs
    pure (lVal, rVal)

runWithDoc_ :: IOSLA (XIOState ()) XmlTree a -> XmlTree -> IO [a]
runWithDoc_ (IOSLA f) doc = snd <$> f (initialState ()) doc

{-
begin point of each edge is estimated from end point and the shape info of the line
so we need to adjust begin points for each line, this is done by picking the closest
"confirmed point" from the estimated begin point.

"confirmed point" includes begin points of a map, and end point of all edges.
-}
adjustLines :: [V2 Int] -> [MyLine] -> [MyLine]
adjustLines startPts ls = adjustLine <$> ls
  where
    confirmedPoints = startPts ++ (_lEnd <$> ls)
    adjustLine :: MyLine -> MyLine
    adjustLine l@(MyLine _ lStartPt _) = l { _lStart = adjustedStartPt }
      where
        adjustedStartPt = minimumBy (compare `on` qdA lStartPt) confirmedPoints

{-
guess names for each node:

- begin nodes are name "<n>" where n is a number,
  in KC3Kai edges.json file, there's no distinction between begin nodes and all are called just "Start".

- for all the other nodes, the name of a node depends on the name of edges pointing to it.
  for an edge with name "line1", this will be "A", and "B" for "line2", "C" for "line3" etc.
  if there are multiple edges pointing to one node, one with the least number wins.

- note that these naming rules are not always working. so one needs to take a closer look on generated data.

-}
mkPointMap :: [V2 Int] -> [MyLine] -> M.Map (V2 Int) String
mkPointMap beginNodes xs = M.union beginNodeNames endNodeNames
  where
    beginNodeNames = M.fromList (zip beginNodes (formatName <$> [1::Int ..]))
      where
        formatName x = "<" ++ show x ++ ">"

    -- collect all possible names and pick the minimal one
    endNodeNames = M.map getMin
                   (M.fromListWith (++)
                    (map convert xs))

    getMin = minimumBy (\x y -> compare (length x) (length y) <> compare x y)
    lineToInt l = read (simpleLName l)
    nodeNameFromInt v
        | v-1 < length ns = ns !! (v-1)
        | otherwise = show v
      where
        ns = map (:[]) ['A'..'Z']

    convert l = (_lEnd l,[nodeNameFromInt .lineToInt $ l])

linesToJSValue :: [MyLine] -> M.Map (V2 Int) String -> JSValue
linesToJSValue xs nnames = JSObject (toJSObject (convert <$> ys))
  where
    ys = sortBy (compare `on` (\l -> read (simpleLName l) :: Int)) xs
    getNm v = makeStart (fromMaybe "Unknown" (M.lookup v nnames))
      where
        makeStart ('<':_) = "Start"
        makeStart v' = v'
    convert :: MyLine -> (String, JSValue)
    convert l = (simpleLName l,JSArray (f <$> [getNm (_lStart l),getNm (_lEnd l)]))
      where
        f = JSString . toJSString

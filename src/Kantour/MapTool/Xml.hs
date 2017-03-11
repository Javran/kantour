{-# LANGUAGE
    Arrows
  , PartialTypeSignatures
  , ScopedTypeVariables
  #-}
module Kantour.MapTool.Xml where

import Data.List
import Text.XML.HXT.Core hiding (when)
import Linear
import Control.Lens hiding (deep)

import Kantour.MapTool.Types

getIntPair :: ArrowXml arr => String -> String -> (Int -> Int -> a) -> arr XmlTree a
getIntPair fstName sndName resultF =
    (   (getAttrValue fstName >>> arr read)
    &&& (getAttrValue sndName >>> arr read)
    ) >>> arr (uncurry resultF)

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
             -< doc
    xMaxMin <- getIntPair "Xmax" "Xmin" (,) -< shape
    yMaxMin <- getIntPair "Ymax" "Ymin" (,) -< shape
    this -< (xMaxMin, yMaxMin)

findSprite :: ArrowXml arr => String -> arr XmlTree XmlTree
findSprite spriteId =
    deep (hasName "item" >>>
          hasAttrValue "spriteId" (== spriteId))

findMapSpriteId :: ArrowXml arr => arr XmlTree String
findMapSpriteId =
    deep (hasName "item"
         >>> hasAttrValue "name" (== "map")
         >>> getAttrValue "characterId")

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

guessStartPoint :: V2 Int -> V2 Int -> ShapeBounds -> V2 Int
guessStartPoint ptEnd sh ((xMax,xMin),(yMax,yMin)) = ptEnd + V2 dx dy
  where
    dx = (view _x sh + ((xMax + xMin) `div` 2)) * 2
    dy = (view _y sh + ((yMax + yMin) `div` 2)) * 2

withMainSprite :: ArrowXml arr => (String -> arr XmlTree a) -> arr XmlTree a
withMainSprite k = proc doc -> do
    mapSpriteId <- findMapSpriteId -< doc
    k mapSpriteId -<< doc

getExtraRoute :: ArrowXml arr => arr XmlTree MyLine
getExtraRoute = proc doc -> do
    (ptExEnd,extraId) <-
        withMainSprite findSprite
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

getMapBeginNode :: ArrowXml arr => String -> arr XmlTree (V2 Int)
getMapBeginNode mapSpriteId = proc doc -> do
    -- load line info
    (ptEnd,spriteId) <-
        findSprite mapSpriteId
        >>> getChild
        >>> hasAttrValue "name" ("line" `isPrefixOf`)
        >>> loadSubMatrixAndId -< doc
    -- select lines whose childrens are without ids
    findSprite spriteId
        >>> ((this /> hasName "subTags")
             `notContaining`
             (this /> hasAttr "characterId")) -<< doc
    this -< ptEnd

getRoute :: ArrowXml arr => String -> arr XmlTree MyLine
getRoute mapSpriteId = proc doc -> do
    (lineName,(ptEnd,lineId)) <-
        findSprite mapSpriteId
        >>> getChild
        >>> hasAttrValue "name" ("line" `isPrefixOf`)
        >>> getAttrValue "name" &&& loadSubMatrixAndId -< doc
    (sb,sh) <- findLineShapeInfo lineId -<< doc
    this -< MyLine lineName (guessStartPoint ptEnd sh sb) ptEnd

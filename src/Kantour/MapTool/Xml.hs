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
import Data.Coerce

import Kantour.MapTool.Types

-- type for the full document
newtype XmlDoc = XmlDoc XmlTree

-- type for sprites
newtype XmlSprite = XmlSprite XmlTree

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

findShapeBounds :: ArrowXml arr => String -> arr XmlDoc ShapeBounds
findShapeBounds shapeId = proc doc -> do
    shape <-
        arr coerce >>>
        deep (hasName "item" >>>
                   hasAttrValue "shapeId" (== shapeId) />
                   hasName "shapeBounds")
             -< doc
    xMaxMin <- getIntPair "Xmax" "Xmin" (,) -< shape
    yMaxMin <- getIntPair "Ymax" "Ymin" (,) -< shape
    this -< (xMaxMin, yMaxMin)

findSprite :: ArrowXml arr => String -> arr XmlDoc XmlSprite
findSprite spriteId =
    arr coerce
    >>> deep (hasName "item" >>> hasAttrValue "spriteId" (== spriteId))
    >>> arr coerce

findMapSpriteId :: ArrowXml arr => arr XmlDoc String
findMapSpriteId =
    arr coerce
    >>> deep
        (hasName "item"
         >>> hasAttrValue "name" (== "map")
         >>> getAttrValue "characterId")

findLineShapeInfo :: ArrowXml arr => String -> arr XmlDoc (ShapeBounds,V2 Int)
findLineShapeInfo lineId = proc doc -> do
    sprite <- findSprite lineId -<< doc
    -- shape should be a child of sprite (the line)
    shapeRef <- getSpriteChild >>> hasAttr "characterId" -< sprite
    (sh,shapeId) <- loadSubMatrixAndId -< shapeRef
    sb <- findShapeBounds shapeId -<< doc
    this -< (sb,sh)

getSpriteChild :: ArrowXml arr => arr XmlSprite XmlTree
getSpriteChild = arr coerce /> hasName "subTags" /> hasName "item"

guessStartPoint :: V2 Int -> V2 Int -> ShapeBounds -> V2 Int
guessStartPoint ptEnd sh ((xMax, xMin), (yMax, yMin)) = ptEnd + V2 dx dy
  where
    dx = (view _x sh + ((xMax + xMin) `div` 2)) * 2
    dy = (view _y sh + ((yMax + yMin) `div` 2)) * 2

getMainSprite :: ArrowXml arr => arr XmlDoc XmlSprite
getMainSprite = proc doc -> do
    mapSpriteId <- findMapSpriteId -< doc
    findSprite mapSpriteId -<< doc

getExtraRoute :: forall arr. ArrowXml arr => arr XmlDoc MyLine
getExtraRoute = proc doc -> do
    (ptExEnd,extraId) <-
        getMainSprite
        >>> getSpriteChild
        >>> loadSubMatrixAndId -< doc
    (lineName, (ptEnd',spriteId)) <-
        findSprite extraId
        >>> getSpriteChild
        >>> hasAttrValue "name" ("line" `isPrefixOf`)
        >>> getAttrValue "name" &&& loadSubMatrixAndId -<< doc
    let ptEnd = ptEnd' ^+^ ptExEnd
    (sb,sh) <- findLineShapeInfo spriteId -<< doc
    this -< MyLine lineName (guessStartPoint ptEnd sh sb) ptEnd

getMapBeginNode :: ArrowXml arr => arr (XmlSprite, XmlDoc) (V2 Int)
getMapBeginNode = proc (spriteRoot, doc) -> do
    -- load line info
    (ptEnd,spriteId) <-
            getSpriteChild
        >>> hasAttrValue "name" ("line" `isPrefixOf`)
        >>> loadSubMatrixAndId -< spriteRoot
    -- select lines whose childrens are without ids
    findSprite spriteId
        >>> arr coerce
        >>> ((this /> hasName "subTags")
             `notContaining`
             (this /> hasAttr "characterId")) -<< doc
    this -< ptEnd

getRoute :: ArrowXml arr => arr (XmlSprite, XmlDoc) MyLine
getRoute = proc (spriteRoot, doc) -> do
    (lineName,(ptEnd,lineId)) <-
        getSpriteChild
        >>> hasAttrValue "name" ("line" `isPrefixOf`)
        >>> getAttrValue "name" &&& loadSubMatrixAndId -< spriteRoot
    (sb,sh) <- findLineShapeInfo lineId -<< doc
    this -< MyLine lineName (guessStartPoint ptEnd sh sb) ptEnd

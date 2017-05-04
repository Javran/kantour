{-# LANGUAGE
    Arrows
  , PartialTypeSignatures
  , ScopedTypeVariables
  #-}
module Kantour.MapTool.Xml where

import Control.Lens hiding (deep)
import Data.Coerce
import Data.List
import Text.XML.HXT.Core hiding (when)
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Linear
import Kantour.MapTool.Types
import Kantour.Utils
import Control.Exception

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

docDeep :: ArrowXml arr => arr XmlTree a -> arr XmlDoc a
docDeep f = arr coerce >>> deep f

findShapeBounds :: ArrowXml arr => String -> arr XmlDoc ShapeBounds
findShapeBounds shapeId = proc doc -> do
    shape <-
        docDeep (hasName "item" >>>
                   hasAttrValue "shapeId" (== shapeId) />
                   hasName "shapeBounds")
             -< doc
    xMaxMin <- getIntPair "Xmax" "Xmin" (,) -< shape
    yMaxMin <- getIntPair "Ymax" "Ymin" (,) -< shape
    this -< (xMaxMin, yMaxMin)

findSprite :: ArrowXml arr => String -> arr XmlDoc XmlSprite
findSprite spriteId =
    docDeep (hasName "item" >>> hasAttrValue "spriteId" (== spriteId))
    >>> arr XmlSprite

findMapSpriteId :: ArrowXml arr => arr XmlDoc String
findMapSpriteId =
    docDeep
        (hasName "item"
         >>> hasAttrValue "name" (== "map")
         >>> getAttrValue "characterId")

findHiddenSpriteId :: ArrowXml arr => arr XmlDoc String
findHiddenSpriteId =
    docDeep (hasName "item"
             >>> hasAttrValue "type" (== "SymbolClassTag"))
    >>> (listA (this /> (hasName "tags" /> getItem)) &&&
         listA (this /> (hasName "names" /> getItem)))
    >>> arr (uncurry findHiddenRoot)
  where
    getItem = hasName "item" /> getText
    findHiddenRoot :: [String] -> [String] -> String
    findHiddenRoot tags names
        | equalLength tags names =
            -- TODO: sometimes the extra data comes directly from SallyMain,
            -- in which case we need to have a more specific filter
            -- (it's 383_17 and 383_29 for 38-3) .. perhaps finding a better parameter syntax would help
            let isExtraRoot (_, s) =
                    "scene.sally.mc.MCCellSP385" `isPrefixOf` s
                    -- "scene.sally.mc.MCCellSP383_17" `isPrefixOf` s
            in case find isExtraRoot (zip tags names) of
                   Just (spriteId, _) -> spriteId
                   Nothing -> error "cell root not found"
        | otherwise = error "tags & names length differs"

findHiddenSpriteRoots :: ArrowXml arr => arr XmlDoc [(String,String)]
findHiddenSpriteRoots =
    docDeep (hasName "item"
             >>> hasAttrValue "type" (== "SymbolClassTag"))
    >>> (listA (this /> (hasName "tags" /> getItem)) &&&
         listA (this /> (hasName "names" /> getItem)))
    >>> arr (uncurry findHiddenRoots)
  where
    getItem = hasName "item" /> getText
    findHiddenRoots :: [String] -> [String] -> [(String,String)]
    findHiddenRoots tags names
        | equalLength tags names =
            let isExtraRoot (_, s) =
                    "scene.sally.mc.MCCellSP" `isPrefixOf` s
            in filter isExtraRoot (zip tags names)
        | otherwise = error "tags & names length differs"

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

getExtraRoute :: forall arr. ArrowXml arr => arr (XmlSprite, XmlDoc) MyLine
getExtraRoute = proc (spriteRoot, doc) -> do
    (ptExEnd,extraId) <-
            getSpriteChild
        >>> hasAttrValue "name" ("extra" `isPrefixOf`)
        >>> loadSubMatrixAndId -< spriteRoot
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

extractFromMain :: ArrowXml arr => arr XmlDoc ([MyLine], [V2 Int])
extractFromMain = proc doc -> do
    mainSprite <- getMainSprite -< doc
    let d' = (mainSprite, doc)
    rs <- listA getRoute -< d'
    extraRs <- listA getExtraRoute -< d'
    begins <- listA getMapBeginNode -< d'
    this -< (rs ++ extraRs, begins)

extractFromHidden :: ArrowXml arr => arr XmlDoc ([MyLine], [V2 Int])
extractFromHidden = proc doc -> do
    spriteId <- findHiddenSpriteId -< doc
    extraSprite <- findSprite spriteId -<< doc
    listA getRoute &&& listA getMapBeginNode -< (extraSprite, doc)

parseXmlDoc
    :: IOSLA (XIOState ()) XmlDoc a
    -> String
    -> IO (Either String [a])
parseXmlDoc arrExtract fp = do
    lDoc <- runX (readDocument [] fp)
    case lDoc of
        [doc] ->
            (Right <$> runWithDoc_ arrExtract doc)
                 `catches`
            [ Handler (\(e :: IOException) -> pure (Left (show e)))
            , Handler (\(e :: ErrorCall) -> pure (Left (show e)))
            ]
        _ -> pure $ Left $ "error when parsing: " ++ fp

safeParseXmlDoc
    :: IOSLA (XIOState ()) XmlDoc ([MyLine], [V2 Int])
    -> String
    -> IO ([MyLine], [V2 Int])
safeParseXmlDoc arrExtract fp = do
    parsed <- right unsafeExactlyOne <$> parseXmlDoc arrExtract fp
    case parsed of
        Left errMsg -> do
            putStrLn $ "Parse error: " ++ errMsg
            pure ([], [])
        Right v -> pure v

runWithDoc_ :: IOSLA (XIOState ()) XmlDoc a -> XmlTree -> IO [a]
runWithDoc_ (IOSLA f) doc = snd <$> f (initialState ()) (XmlDoc doc)

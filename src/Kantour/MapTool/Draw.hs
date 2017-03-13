{-# LANGUAGE
    NoMonomorphismRestriction
  , FlexibleContexts
  , TypeFamilies
  #-}
module Kantour.MapTool.Draw where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Coerce

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Kantour.MapTool.Types

{-# ANN module "Hlint: ignore Avoid lambda" #-}

twipToPixel :: Integral i => i -> Double
twipToPixel x = fromIntegral x / 20

drawKCMap :: MapInfo
          -> Diagram B
drawKCMap mi@MapInfo {_miLines = xs} =
    (allPoints # applyAll routeConnections) `atop` arrTextLayer
  where
    arrowOpts = with & gaps .~ small & headLength .~ 22
    circle' color txt =
        text txt # fontSizeL 10 # fc black <> circle 10 # fc color
    allPoints :: Diagram B
    allPoints = position (renderPoint <$> (M.keys . _miNodeNames $ mi))
      where
        renderPoint p = (convertPt p, circle' color nodeName # named nodeName)
          where
            color =
                if p `S.member` _miStarts mi
                    then red
                    else green
            nodeName = getNodeName p mi
    routeConnections =
        map (connectOutside' arrowOpts <$> lineStartName <*> lineEndName) xs
    arrTextLayer = position (zip (lineMid <$> xs) arrLbls)
      where
        arrLbls = map (\l -> text (simpleLName l) # fc blue # fontSizeL 16) xs
    lineStartName :: MyLine -> String
    lineStartName = (\pt -> getNodeName pt mi) . _lStart
    lineEndName = (\pt -> getNodeName pt mi) . _lEnd

draw :: MapInfo -> IO ()
draw = mainWith . drawKCMap

lineMid :: MyLine -> Point V2 Double
lineMid (MyLine _ pStart pEnd) = (convertPt pStart ^+^ convertPt pEnd) ^/ 2

convertPt :: V2 Int -> Point V2 Double
convertPt = reflectY . coerce . (twipToPixel <$>)

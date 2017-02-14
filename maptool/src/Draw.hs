{-# LANGUAGE
    NoMonomorphismRestriction
  , FlexibleContexts
  , TypeFamilies
  #-}
module Draw where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Coerce

import Types

twipToPixel :: Integral i => i -> Double
twipToPixel x = fromIntegral x / 20

drawKCMap :: [MyLine] -> M.Map (V2 Int) String -> Diagram B
drawKCMap xs pm = ((endPoints <> startPoints)
                   # applyAll
                     (map (\l ->
                           connectOutside' arrowOpts (lineStartName l) (lineEndName l))
                      xs))
                  `atop`
                  position (zip (lineMid <$> xs) arrLbls)
  where
    circle' color txt = text txt # fontSizeL 10 # fc black <> circle 10 # fc color
    mkPoints getter color lineName =
        atPoints
          (fmap (convertPt . getter) xs)
          (map (\l ->
                circle' color (fromMaybe "?" (M.lookup (getter l) pm))
                # named (lineName l)) xs)
    endPoints = mkPoints _lEnd green lineEndName
    startPoints = mkPoints _lStart red lineStartName
    arrowOpts = with & gaps .~ small & headLength .~ 22
    arrLbls = map (\l -> text (simpleLName l) # fc blue # fontSizeL 16) xs
    lineStartName = (++ "r") . _lName
    lineEndName = (++ "g") . _lName

draw :: [MyLine] -> M.Map (V2 Int) String -> IO ()
draw xs pm = mainWith (drawKCMap xs pm)

lineMid :: MyLine -> Point V2 Double
lineMid (MyLine _ pStart pEnd) = (convertPt pStart ^+^ convertPt pEnd) ^/ 2

convertPt :: V2 Int -> Point V2 Double
convertPt = reflectY . coerce . (twipToPixel <$>)

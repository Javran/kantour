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
drawKCMap xs pm = (mconcat [greenPoints, redPoints]
                   # applyAll [connectOutside' arrowOpts (ln ++ "r") (ln ++ "g")
                              | ln <- map _lName xs])
                  `atop`
                  position (zip midPoints arrLbls)
  where
    myCircle color txt = text txt # fontSizeL 10 # fc black <> circle 10 # fc color
    cirGreen = myCircle green
    cirRed = myCircle red
    greenPoints = atPoints
                  (fmap (convertPt . _lEnd) xs)
                  (map (\l ->
                        cirGreen (fromMaybe "?" (M.lookup (_lEnd l) pm))
                        # named (_lName l ++ "g")) xs)
    redPoints = atPoints
                (fmap (convertPt . fromJust . _lStart) xs)
                (map (\l -> cirRed (fromMaybe "?" (M.lookup (fromJust $ _lStart l) pm))
                            # named (_lName l ++ "r")) xs)
    arrowOpts = with & gaps .~ small & headLength .~ 22
    midPoints :: [Point V2 Double]
    midPoints = lineMid <$> xs
    arrLbls = map (\l -> text (simpleLName l) # fc blue # fontSizeL 16) xs

draw :: [MyLine] -> M.Map (V2 Int) String -> IO ()
draw xs pm = mainWith (drawKCMap xs pm)

lineMid :: MyLine -> Point V2 Double
lineMid (MyLine _ (Just pStart) pEnd) = (convertPt pStart ^+^ convertPt pEnd) ^/ 2
lineMid (MyLine _ Nothing pEnd) = convertPt pEnd

convertPt :: V2 Int -> Point V2 Double
convertPt = reflectY . coerce . (twipToPixel <$>)

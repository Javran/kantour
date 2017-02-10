{-# LANGUAGE
    NoMonomorphismRestriction
  , FlexibleContexts
  , TypeFamilies
  #-}
module Draw where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Maybe

import Types

drawKCMap :: V2 Int -> [MyLine] -> Diagram B
drawKCMap st xs = reflectY (mconcat [greenPoints, redPoints])
                  # applyAll [connectOutside' arrowOpts (ln ++ "r") (ln ++ "g") | ln <- lNames]
  where
    lNames = map _lName xs
    cirGreen = circle 5 # fc green
    cirRed = circle 5 # fc red
    greenPoints = flip atPoints (cirGreen
                                 : map (\lineName ->
                                        cirGreen # named (lineName ++ "g")) lNames)
                  $ map p2 (convertPt <$> (st : map _lEnd xs))
    redPoints = flip atPoints (map (\lineName -> cirRed # named (lineName ++ "r")) lNames)
                $ map p2 (convertPt <$> map (fromJust . _lStart) xs)
    convertPt (V2 x y) = (fromIntegral x / 20, fromIntegral y / 20)
    arrowOpts = with & gaps .~ small & headLength .~ 22

draw :: V2 Int -> [MyLine] -> IO ()
draw st xs = mainWith (drawKCMap st xs)

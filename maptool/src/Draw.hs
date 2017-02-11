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

import Types

drawKCMap :: [MyLine] -> M.Map (V2 Int) String -> Diagram B
drawKCMap xs pm = (mconcat [greenPoints, redPoints]
                # applyAll [connectOutside' arrowOpts (ln ++ "r") (ln ++ "g")
                           | ln <- map _lName xs]) `atop` position (zip midPoints arrLbls)
  where
    cirGreen txt = (text txt # fontSizeL 10 # fc black) <> (circle 10 # fc green)
    cirRed txt = (text txt # fontSizeL 10 # fc black) <> (circle 10 # fc red)
    greenPoints = flip atPoints (map (\l ->
                                      cirGreen (fromMaybe "?" (M.lookup (_lEnd l) pm))
                                                # named (_lName l ++ "g")) xs)
                  $ reflectY ( map p2 (convertPt <$> map _lEnd xs))
    redPoints = flip atPoints (map (\l -> cirRed (fromMaybe "?" (M.lookup (fromJust $ _lStart l) pm))
                               # named (_lName l ++ "r")) xs)
                $ reflectY ( map p2 (convertPt <$> map (fromJust . _lStart) xs))
    convertPt (V2 x y) = (fromIntegral x / 20, fromIntegral y / 20)
    arrowOpts = with & gaps .~ small & headLength .~ 22
    midPoints :: [Point V2 Double]
    midPoints = reflectY <$> (((\pt -> (pt^._x) ^& (pt^._y)) . lineMid) <$> xs)
    arrLbls = map (\l -> text (drop 4 $ _lName l) # fc blue # fontSizeL 16) xs

draw :: [MyLine] -> M.Map (V2 Int) String -> IO ()
draw xs pm = mainWith (drawKCMap xs pm)

lineMid :: MyLine -> V2 Double
lineMid (MyLine _ (Just pStart) pEnd) = ((\x -> fromIntegral x / 20) <$> (pStart ^+^ pEnd)) ^/ 2

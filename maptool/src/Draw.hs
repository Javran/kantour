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
                  (reflectY (fmap (convertPt . _lEnd) xs))
                  (map (\l ->
                        cirGreen (fromMaybe "?" (M.lookup (_lEnd l) pm))
                        # named (_lName l ++ "g")) xs)
    redPoints = atPoints
                (reflectY (fmap (convertPt . fromJust . _lStart) xs))
                (map (\l -> cirRed (fromMaybe "?" (M.lookup (fromJust $ _lStart l) pm))
                            # named (_lName l ++ "r")) xs)
    convertPt p = coerce (twipToPixel <$> p)
    arrowOpts = with & gaps .~ small & headLength .~ 22
    midPoints :: [Point V2 Double]
    midPoints = reflectY <$> ((coerce . lineMid) <$> xs)
    arrLbls = map (\l -> text (simpleLName l) # fc blue # fontSizeL 16) xs

draw :: [MyLine] -> M.Map (V2 Int) String -> IO ()
draw xs pm = mainWith (drawKCMap xs pm)

lineMid :: MyLine -> V2 Double
lineMid (MyLine _ (Just pStart) pEnd) = (twipToPixel <$> (pStart ^+^ pEnd)) ^/ 2
lineMid (MyLine _ Nothing pEnd) = twipToPixel <$> pEnd

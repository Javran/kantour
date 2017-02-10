{-# LANGUAGE
    NoMonomorphismRestriction
  , FlexibleContexts
  , TypeFamilies
  #-}
module Draw where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Maybe
import Data.Foldable
import Data.Function

data MyPoint = MyPoint
  { ptX :: Int
  , ptY :: Int
  } deriving Show

data MyLine = MyLine
  { lName :: String
  , lStart :: Maybe MyPoint
  , lEnd :: MyPoint
  } deriving Show


drawKCMap :: MyPoint -> [MyLine] -> Diagram B
drawKCMap st xs = reflectY (mconcat [greenPoints, redPoints])
                  # applyAll [connectOutside' arrowOpts (ln ++ "r") (ln ++ "g") | ln <- lNames]
  where
    lNames = map lName xs
    cirGreen = circle 5 # fc green
    cirRed = circle 5 # fc red
    greenPoints = flip atPoints (cirGreen
                                 : map (\lineName ->
                                        cirGreen # named (lineName ++ "g")) lNames)
                  $ map p2 (convertPt <$> (st : map lEnd xs))
    redPoints = flip atPoints (map (\lineName -> cirRed # named (lineName ++ "r")) lNames)
                $ map p2 (convertPt <$> map (fromJust . lStart) xs)
    convertPt (MyPoint x y) = (fromIntegral x / 20, fromIntegral y / 20)
    arrowOpts = with & gaps .~ small & headLength .~ 22

draw :: MyPoint -> [MyLine] -> IO ()
draw st xs = mainWith (drawKCMap st xs')
  where
    xs' = adjustLines st xs

ptDistanceSq :: MyPoint -> MyPoint -> Int
ptDistanceSq p1 p2 = dx*dx + dy*dy
  where
    dx = ptX p1 - ptX p2
    dy = ptY p1 - ptY p2

adjustLines :: MyPoint -> [MyLine] -> [MyLine]
adjustLines startPt lines = adjustLine <$> lines
  where
    confirmedPoints = startPt : (lEnd <$> lines)
    adjustLine :: MyLine -> MyLine
    adjustLine l@(MyLine _ Nothing _) = l
    adjustLine l@(MyLine _ (Just lStartPt) lEndPt) = l { lStart = Just adjustedStartPt }
      where
        adjustedStartPt = minimumBy (compare `on` (lStartPt `ptDistanceSq`)) confirmedPoints

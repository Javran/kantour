module Main where

import System.Environment
import Control.Monad
import Control.Monad.Primitive
import System.Random.MWC
import Text.Printf
import qualified Data.Vector as V
import Statistics.Sample
import Data.Vector.Algorithms.Intro
import Data.Function
import Numeric.SpecFunctions

-- either a number between 0 and 1
-- or {x}% where x can be between 0 and 100
parseRate :: String -> Maybe Double
parseRate [] = Nothing
parseRate xs
    | last xs == '%'
    , [(r,"")] <- reads (init xs) = Just (r/100) >>= verify
    | [(r,"")] <- reads xs = Just r >>= verify
    | otherwise = Nothing
  where
    verify x = x <$ guard (x >= 0 && x <= 1)

-- must be positive number
parseCount :: String -> Maybe Int
parseCount xs
    | [(v,"")] <- reads xs
    , v > 0 = Just v
    | otherwise = Nothing

experiment :: PrimMonad m => Gen (PrimState m) -> Double -> m Int
experiment g r = experiment' 0
  where
    experiment' cnt = do
        let newCnt = cnt+1
        v <- uniform g
        if v <= r
          then pure newCnt
          else experiment' newCnt

computeCDF :: Int -> Int -> Double -> Double
computeCDF k r p = i (1-p) (fromIntegral r) (fromIntegral (k+1))
  where
    i x a b = incompleteBeta a b x

analyzeResult :: Int -> Double -> [Int] -> IO ()
analyzeResult _ _ [] = putStrLn "Result is empty."
analyzeResult tot p xs = do
    let _xs = V.fromList xs :: V.Vector Int
    ys' <- V.unsafeThaw _xs
    sort ys'
    -- now ys is the sorted version of xs
    ys <- V.unsafeFreeze ys'
    let xMin = V.head ys
        xMax = V.last ys
        ysDb = fromIntegral <$> ys :: V.Vector Double
    printf "min: %d, max: %d\n" xMin xMax
    printf "mean: %6.4f, std dev.: %6.4f\n" (mean ysDb) (stdDev ysDb)
    (fix $ \indScan curInd ->
      if curInd >= V.length ys
        then pure ()
        else do
          let curE = ys V.! curInd
          -- find next index whose elem is not curE
          let nextInd = fix (\self curInd' ->
                             if curInd' >= V.length ys
                               then Nothing
                               else if ys V.! curInd' /= curE
                                      then Just curInd'
                                      else self (curInd'+1)) (curInd+1)
              fI = fromIntegral :: Int -> Double
              ppr n cr = when (n `mod` 10 == 0) $
                  printf "<= %d\tactual: %6.4f%%\tcomputed: %6.4f%%\n"
                      n
                      (cr * 100)
                      (computeCDF n 1 (1-p) * 100)
          case nextInd of
              Nothing ->
                  ppr curE (fI (V.length ys -1) / fI tot )
              Just ind -> do
                  ppr curE (fI (ind-1) / fI tot)
                  indScan ind
          ) 0

main :: IO ()
main = do
    [rawDropRate,rawCnt] <- getArgs
    case (,) <$> parseRate rawDropRate <*> parseCount rawCnt of
        Just (dropRate,cnt) -> do
            printf "Performing %d experiments with drop rate %6.4f%%:\n" cnt (dropRate * 100)
            g <- createSystemRandom
            results <- replicateM cnt (experiment g dropRate)
            analyzeResult cnt dropRate results
        Nothing -> do
            putStrLn "Usage: <rate> <# of experiments>"
            putStrLn "example of rate: '0.20' or '20%'"

    pure ()

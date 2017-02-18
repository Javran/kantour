module Main where

import System.Environment
import Control.Monad
import Control.Monad.Primitive
import System.Random.MWC
import Text.Printf

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

analyzeResult :: [Int] -> IO ()
analyzeResult [] = putStrLn "Result is empty."
analyzeResult xs = do
    let xMin = minimum xs
        xMax = maximum xs

    printf "min: %d, max: %d\n" xMin xMax

main :: IO ()
main = do
    [rawDropRate,rawCnt] <- getArgs
    case (,) <$> parseRate rawDropRate <*> parseCount rawCnt of
        Just (dropRate,cnt) -> do
            printf "Performing %d experiments with drop rate %6.4f%%:\n" cnt (dropRate * 100)
            g <- createSystemRandom
            results <- replicateM cnt (experiment g dropRate)
            analyzeResult results
        Nothing -> do
            putStrLn "Usage: <rate> <# of experiments>"
            putStrLn "example of rate: '0.20' or '20%'"

    pure ()

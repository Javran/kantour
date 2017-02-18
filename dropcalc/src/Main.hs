module Main where

import System.Environment
import Control.Monad

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

main :: IO ()
main = do
    [rawDropRate,rawCnt] <- getArgs
    case (,) <$> parseRate rawDropRate <*> parseCount rawCnt of
        Just _ -> pure ()
        Nothing -> do
            putStrLn "Usage: <rate> <# of experiments>"
            putStrLn "example of rate: '0.20' or '20%'"
    pure ()

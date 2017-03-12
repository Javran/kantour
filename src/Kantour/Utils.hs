module Kantour.Utils where

import Data.Foldable
import Control.Monad
import Data.List
import Data.Char

-- well, I don't think removing some extra parameters would make anyone look smarter.
{-# ANN module "HLint: ignore Eta reduce" #-}

alterAL :: Eq k => (Maybe v -> Maybe v) -> k -> [(k,v)] -> [(k,v)]
alterAL modify k [] = case modify Nothing of
    Nothing -> []
    Just v -> [(k,v)]
alterAL modify k ((xk,xv):xs)
    | k == xk = case modify (Just xv) of
        Nothing -> xs
        Just xv' -> (xk,xv') : xs
    | otherwise = (xk,xv) : alterAL modify k xs

insertAL :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
insertAL k v = alterAL (const (Just v)) k

deleteAL :: Eq k => k -> [(k,v)] -> [(k,v)]
deleteAL k = alterAL (const Nothing) k

unionAL :: Eq k => [(k,v)] -> [(k,v)] -> [(k,v)]
unionAL = foldl' (\acc (k,v) -> insertAL k v acc)

-- a more informative version of isPrefixOf
removePrefix :: String -> String -> Maybe String
removePrefix xs ys = do
    guard $ xs `isPrefixOf` ys
    pure (drop (length xs) ys)

stripL :: String -> String
stripL = dropWhile isSpace

stripR :: String -> String
stripR xs = case remained of
    [] -> part1
    _ -> part1 ++ part2 ++ stripR remained
 where
   -- INVARIANT:
   -- xs == part1 ++ part2 ++ remained
   -- part1 are all non-spaces
   -- part2 are all spaces
   -- remained begins with non-space
   (part1, sp) = break isSpace xs
   (part2, remained) = span isSpace sp

strip :: String -> String
strip =
    -- stripL first so we can leave
    -- the longest non-space leading chunk to stripR
    stripR . stripL

-- from GHC Util.hs
equalLength :: [a] -> [b] -> Bool
equalLength [] [] = True
equalLength (_:xs) (_:ys) = equalLength xs ys
equalLength _ _ = False

compareLength :: [a] -> [b] -> Ordering
compareLength [] [] = EQ
compareLength (_:xs) (_:ys) = compareLength xs ys
compareLength [] _ = LT
compareLength _ [] = GT

module Kantour.Utils where

import Data.Foldable
import Control.Monad
import Data.List

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

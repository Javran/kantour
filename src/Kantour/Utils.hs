module Kantour.Utils where

alterAL :: Eq k => (Maybe v -> Maybe v) -> k -> [(k,v)] -> [(k,v)]
alterAL modify k [] = case modify Nothing of
    Nothing -> []
    Just v -> [(k,v)]
alterAL modify k ((xk,xv):xs)
    | k == xk = case modify (Just xv) of
        Nothing -> xs
        Just xv' -> (xk,xv') : xs
    | otherwise = (xk,xv) : alterAL modify k xs

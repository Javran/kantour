module Kantour.Utils where

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

onePassInsert :: Eq k => [(k,v)] -> [(k,v)] -> ([(k,v)],[(k,v)])
onePassInsert xs@((xk,xv):xs') ys@((yk,yv):ys')
    | xk == yk =
        let (result,todo) = onePassInsert xs' ys'
        in ((yk,yv):result,todo)
    | otherwise =
        let (result,todo) = onePassInsert xs' ys
        in ((xk,xv):result,todo)
onePassInsert [] ys = ([],ys)
onePassInsert xs [] = (xs,[])

unionAL :: Eq k => [(k,v)] -> [(k,v)] -> [(k,v)]
unionAL xs ys =
    let (result,todo) = onePassInsert xs ys
    in if null todo
          then result
          else if length ys == length todo
                  then result ++ todo
                  else unionAL result todo

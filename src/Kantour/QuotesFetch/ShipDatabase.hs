{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Kantour.QuotesFetch.ShipDatabase where

import Language.Lua
import Data.Monoid
import Data.Coerce
import qualified Data.Text as T

import Kantour.QuotesFetch.Fetch
import Kantour.QuotesFetch.Types

-- we can probably use "hslua" to properly interpret everything,
-- but in order to do so we need way more complicated code than this,
-- so for now let's better just live with it.

fetchDatabase :: IO ShipDatabase
fetchDatabase = do
    content <- fetchWikiLink "模块:舰娘数据"
    let (TableConst xs) = getRawDatabase (T.pack content)
    pure (ShipDb xs)

-- search the first assign that looks like "_.shipDataTb = dbRaw"
-- and retrieve the expression on RHS
getRawDatabase :: T.Text -> Exp
getRawDatabase raw = dbRaw
  where
    Right (Block stats _) = parseText chunk raw
    Just dbRaw = coerce (foldMap (coerce isTarget) stats :: Alt Maybe Exp)
      where
        isTarget (Assign [SelectName _ (Name n)] [tbl@TableConst {}])
            | n == "shipDataTb"= Just tbl
        isTarget _ = Nothing

-- lookup bindings in a lua table
luaLookup :: String -> Exp -> Maybe Exp
luaLookup k (TableConst xs) = coerce (foldMap (coerce check) xs :: Alt Maybe Exp)
  where
    check (ExpField (String ek) ev)
        | ek == T.pack k = Just ev
    check _ = Nothing
luaLookup _ _ = error "luaLookup: not a table"

printKeys :: Exp -> IO ()
printKeys (TableConst xs) = mapM_ ppr xs
  where
    ppr (ExpField (String e) tbl) = do
        let v = luaLookup "\"ID\"" tbl
        putStrLn $ T.unpack e ++ " => " ++ show v
    ppr e = putStrLn $ "Unexpected structure: " ++ show e
printKeys _ = error "printKeys: not a table"

findMasterId :: String -> ShipDatabase -> MasterId
findMasterId cid (ShipDb xs) = read (T.unpack x)
  where
    Just shipInfo = luaLookup ("\"" ++ cid ++ "\"") (TableConst xs)
    Just (Number x) = luaLookup "\"ID\"" shipInfo

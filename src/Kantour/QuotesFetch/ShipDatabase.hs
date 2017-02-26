{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Kantour.QuotesFetch.ShipDatabase where

import Language.Lua
import Data.Monoid
import Data.Maybe
import Data.Coerce
import qualified Data.Text as T

import Kantour.QuotesFetch.Types

-- we can probably use "hslua" to properly interpret everything,
-- but in order to do so we need way more complicated code than this,
-- so for now let's better just live with it.

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

getDatabase :: String -> ShipDatabase
getDatabase raw = ShipDb xs
  where
    (TableConst xs) = getRawDatabase (T.pack raw)

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

findShipName :: ShipDatabase -> MasterId -> (String, String)
findShipName (ShipDb db) mstId = fromJust (lookup mstId revTbl)
  where
    convert :: TableField -> Maybe (MasterId, (String,String))
    convert (ExpField _ ev) = do
        let deConst = init . tail
        (Number raw) <- luaLookup "\"ID\"" ev
        let mId = read (T.unpack raw)
        (String jp) <- luaLookup "\"日文名\"" ev
        (String scn) <- luaLookup "\"中文名\"" ev
        pure (mId, (deConst $ T.unpack jp,deConst $ T.unpack scn))
    convert _ = Nothing
    revTbl = mapMaybe convert db

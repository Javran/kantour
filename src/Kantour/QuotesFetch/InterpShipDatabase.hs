module Kantour.QuotesFetch.InterpShipDatabase
  ( ShipDatabase
  , ShipData(..)

  , libIdToMasterId
  , masterIdToLibId
  , getOrigins

  , shipDatabaseFromString
  ) where

import Data.Function
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Foldable
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import qualified Scripting.Lua as Lua
import Kantour.QuotesFetch.Types
import Data.Maybe
import Text.PrettyPrint.HughesPJClass

import qualified Data.IntMap as IM
import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS

{-# ANN module "HLint: ignore Avoid lambda" #-}
{-# ANN module "HLint: ignore Use unless" #-}

data ShipData = ShipData
  { sdLibraryId :: LibraryId
  , sdMasterId :: MasterId
  , sdNameJP :: String
  , sdNameSCN :: String
  , sdRemodelBefore :: Maybe LibraryId
  , sdRemodelAfter :: Maybe LibraryId
  } deriving Show

data ShipDatabase = ShipDatabase
  { sdbMstTable :: IM.IntMap ShipData
  , sdbLibTable :: M.Map LibraryId ShipData
  , sdbOrigins :: IS.IntSet -- MasterId for ships that does not have "remodelBefore"
  }

instance Pretty ShipData where
    pPrint sd =
        hang title 2
        $ vcat [ text "Name (JP):" <+> text (sdNameJP sd)
               , text "Name (SCN):" <+> text (sdNameSCN sd)
               , text "Before:" <+> maybe (text "-") text (sdRemodelBefore sd)
               , text "After:" <+> maybe (text "-") text (sdRemodelAfter sd)
               ]
      where
        title = int (sdMasterId sd) <+> parens (text (sdLibraryId sd))

shipDatabaseFromString :: Bool -> String -> IO ShipDatabase
shipDatabaseFromString needVerify raw = do
    sdb <- mkShipDatabase <$> interpShipDataList raw
    when needVerify (checkShipDatabase sdb)
    pure sdb

interpShipDataList :: String -> IO [ShipData]
interpShipDataList src = do
    l <- Lua.newstate
    Lua.openlibs l
    eCod <- Lua.loadstring l src "shipinfo"
    guard (eCod == 0)
    Lua.call l 0 1
    Lua.getfield l (-1) "shipDataTb"
    xs <- enumTable l (runMaybeT $ do
        libId <- MaybeT $ getStr l (-2)
        scnName <- MaybeT $ getStrField l "中文名"
        jpName <- MaybeT $ getStrField l "日文名"
        mstId <- MaybeT $ getIntField l "ID"
        (remodelBefore,remodelAfter) <- MaybeT $ do
            Lua.getfield l (-1) "改造"
            mRemodelBefore <- liftIO $ getStrField l "改造前"
            mRemodelAfter <- liftIO $ getStrField l "改造后"
            Lua.pop l 1
            pure ((,) <$> mRemodelBefore <*> mRemodelAfter)
        let toLibId "-1" = Nothing
            toLibId x = Just x
        pure (ShipData libId (fromIntegral mstId) jpName scnName
              (toLibId remodelBefore) (toLibId remodelAfter)))
    Lua.close l
    pure (catMaybes xs)

getIntField :: Lua.LuaState -> String -> IO (Maybe Lua.LuaInteger)
getIntField l fldName = do
    Lua.getfield l (-1) fldName
    x <- getInt l (-1)
    Lua.pop l 1
    pure x

getStrField :: Lua.LuaState -> String -> IO (Maybe String)
getStrField l fldName = do
    Lua.getfield l (-1) fldName
    x <- getStr l (-1)
    Lua.pop l 1
    pure x

getStr :: Lua.LuaState -> Int -> IO (Maybe String)
getStr l ind = do
    kt <- Lua.ltype l ind
    if kt == Lua.TSTRING
      then Just . T.unpack . decodeUtf8 <$> Lua.tostring l ind
      else pure Nothing

getInt :: Lua.LuaState -> Int -> IO (Maybe Lua.LuaInteger)
getInt l ind = do
    kt <- Lua.ltype l ind
    if kt == Lua.TNUMBER
      then Just <$> Lua.tointeger l ind
      else pure Nothing

enumTable :: Lua.LuaState -> IO a -> IO [a]
enumTable l action = do
    Lua.pushnil l
    fix $ \self -> do
        flg <- Lua.next l (-2)
        if flg
          then do
            -- key at -2
            -- value at -1
            x <- action
            Lua.pop l 1
            xs <- self
            pure (x : xs)
          else pure []

mkShipDatabase :: [ShipData] -> ShipDatabase
mkShipDatabase xs = ShipDatabase msts libs origins
  where
    msts = IM.fromList (map (\x -> (sdMasterId x, x)) xs)
    libs = M.fromList (map (\x -> (sdLibraryId x, x)) xs)
    origins =
        IS.fromList
        . map sdMasterId
        . filter (isNothing . sdRemodelBefore)
        $ xs

masterIdToLibId :: ShipDatabase -> MasterId -> LibraryId
masterIdToLibId sdb mstId =
    sdLibraryId (findByMstId sdb mstId)

libIdToMasterId :: ShipDatabase -> LibraryId -> MasterId
libIdToMasterId sdb libId =
    sdMasterId (findByLibId sdb libId)

findByMstId :: ShipDatabase -> MasterId -> ShipData
findByMstId sdb mstId = fromJust (IM.lookup mstId (sdbMstTable sdb))

findByLibId :: ShipDatabase -> LibraryId -> ShipData
findByLibId sdb libId = fromJust (M.lookup libId (sdbLibTable sdb))

checkShipDatabase :: ShipDatabase -> IO ()
checkShipDatabase sdb = do
    let collectRemodelChain cur mstSet =
            case sdRemodelAfter (findByMstId sdb cur) of
                Nothing -> set'
                Just next | nextMst <- libIdToMasterId sdb next ->
                    if nextMst `IS.member` set'
                      then set'
                      else collectRemodelChain nextMst set'
          where
            set' = IS.insert cur mstSet
        coveredSet =
              foldl' IS.union IS.empty
            . map (\x -> collectRemodelChain x IS.empty)
            . IS.toList
            $ sdbOrigins sdb
        missingSet = IM.keysSet (sdbMstTable sdb) `IS.difference` coveredSet
    when (not (IS.null missingSet)) $
        putStrLn $ "MasterIds not in any remodel chain: " ++ show missingSet

findShipName :: ShipDatabase -> MasterId -> (String, String)
findShipName sdb mstId = (sdNameJP sd , sdNameSCN sd)
  where
    sd = findByMstId sdb mstId

getOrigins :: ShipDatabase -> IS.IntSet
getOrigins = sdbOrigins

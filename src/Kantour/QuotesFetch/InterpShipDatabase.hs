module Kantour.QuotesFetch.InterpShipDatabase where

import Data.Function
import qualified Data.Text as T
import Data.Text.Encoding
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import qualified Scripting.Lua as Lua
import Kantour.QuotesFetch.Types
import Data.Maybe
import Text.PrettyPrint.HughesPJClass

data ShipInfo = ShipInfo
  { siLibraryId :: LibraryId
  , siMasterId :: MasterId
  , siNameJP :: String
  , siNameSCN :: String
  , siRemodelBefore :: Maybe LibraryId
  , siRemodelAfter :: Maybe LibraryId
  } deriving Show

instance Pretty ShipInfo where
    pPrint si =
        hang title 2
        $ vcat [ text "Name (JP):" <+> text (siNameJP si)
               , text "Name (SCN):" <+> text (siNameSCN si)
               , text "Before:" <+> maybe (text "-") text (siRemodelBefore si)
               , text "After:" <+> maybe (text "-") text (siRemodelAfter si)
               ]
      where
        title = int (siMasterId si) <+> parens (text (siLibraryId si))

interpShipInfoList :: String -> IO [ShipInfo]
interpShipInfoList src = do
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
        pure (ShipInfo libId (fromIntegral mstId) jpName scnName
              (toLibId remodelBefore) (toLibId remodelAfter)))
    Lua.close l
    pure (catMaybes xs)

getIntField :: Lua.LuaState -> String -> IO (Maybe Lua.LuaInteger)
getIntField l fldName = do
    Lua.getfield l (-1) fldName
    x <- getInt l (-1)
    Lua.pop l 1
    pure x

getNumField :: Lua.LuaState -> String -> IO (Maybe Lua.LuaNumber)
getNumField l fldName = do
    Lua.getfield l (-1) fldName
    x <- getNum l (-1)
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

getNum :: Lua.LuaState -> Int -> IO (Maybe Lua.LuaNumber)
getNum l ind = do
    kt <- Lua.ltype l ind
    if kt == Lua.TNUMBER
      then Just <$> Lua.tonumber l ind
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

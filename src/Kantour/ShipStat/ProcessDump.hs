{-# LANGUAGE
    ScopedTypeVariables
  , DeriveGeneric
  , OverloadedStrings
  , DeriveFunctor
  #-}
module Kantour.ShipStat.ProcessDump where

import GHC.Generics
import Text.Printf
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import System.Exit
import Data.Traversable
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Control.Monad
import Control.Arrow
import Data.Coerce
import qualified Data.IntMap.Strict as IM
import Kantour.ShipStat.Core

data StatStruct a = StatStruct
  { _evasion :: a
  , _los :: a
  , _asw :: a
  } deriving (Eq, Functor)

newtype ShipStatRecord =
    ShipStatRecord (Int, (Int, StatStruct Int)) deriving Generic

instance FromJSON ShipStatRecord where
    parseJSON (Object v) = do
        shipId <- v .: "id"
        evasion <- v .: "evasion"
        asw <- v .: "asw"
        los <- v .: "los"
        lv <- v .: "lv"
        pure $ ShipStatRecord (shipId, (lv, StatStruct evasion los asw))
    parseJSON invalid = typeMismatch "ShipStatRecord" invalid

-- key: level
type ShipLevelRecords = IM.IntMap (StatStruct Int)
-- key: shipId, value: level records
type ShipRecords = IM.IntMap ShipLevelRecords

processShipLevelRecords :: ShipLevelRecords -> Maybe (StatStruct [StatInfo])
processShipLevelRecords records
    | IM.size records >= 2 = pure $ computeStatInfo <$> StatStruct evsRecords losRecords aswRecords
    | otherwise = Nothing
  where
    evsRecords = IM.map _evasion records
    losRecords = IM.map _los records
    aswRecords = IM.map _asw records

groupDumpInfoWithWarning :: [ShipStatRecord] -> IO (IM.IntMap ShipLevelRecords)
groupDumpInfoWithWarning rs = do
    let records :: [(Int, (Int, StatStruct Int))]
        records = coerce rs
        grp1 :: IM.IntMap [(Int, StatStruct Int)]
        grp1 = IM.fromListWith (++) $ (map . second) pure records
        traverseShipLevelRecords :: Int -> [(Int, StatStruct Int)] -> IO ShipLevelRecords
        traverseShipLevelRecords shipId lvlRecords = do
            let groupped :: IM.IntMap [StatStruct Int]
                groupped = IM.fromListWith (++) $ (map . second) pure lvlRecords
                isAllEqual :: forall a. Eq a => [a] -> Maybe a
                isAllEqual xs = case xs of
                    [] -> Nothing
                    (y:ys) -> guard (all (== y) ys) >> pure y
            (ys :: IM.IntMap (Maybe (StatStruct Int))) <- for groupped $ \xs ->
                case isAllEqual xs of
                    Nothing -> do
                        putStrLn $ "warning: inconsistent data found for ship " ++ show shipId
                        pure Nothing
                    Just x -> pure (Just x)
            let collapse (x, Just y) = [(x, y)]
                collapse _ = []
            pure $ IM.fromList $ concatMap collapse $ IM.toList ys
    IM.traverseWithKey traverseShipLevelRecords grp1

processDump :: [String] -> IO ()
processDump [fp] = do
    (rawLines :: [Maybe ShipStatRecord]) <-
        fmap decode . LBSC.lines <$> LBS.readFile fp
    let lineCount = length rawLines
        xs = catMaybes rawLines
        validLineCount = length xs
    when (validLineCount /= lineCount) $
      putStrLn $ "invalid lines: " ++ show (lineCount - validLineCount)
    shipStatInfoTable <- IM.map processShipLevelRecords <$> groupDumpInfoWithWarning xs
    _ <- flip IM.traverseWithKey shipStatInfoTable $ \shipId statInfoRowsM ->
        case statInfoRowsM of
            Just statInfoRows -> do
                let StatStruct {_evasion = eInfo, _asw = aInfo, _los = lInfo } = statInfoRows
                    statInfoStr (StatInfo b d) = printf "base: %d, lv.99: %d" b (b+d)
                    pprInfo :: [StatInfo] -> IO ()
                    pprInfo [] = putStrLn "    <inconsistent>"
                    pprInfo ss = forM_ ss $ \si -> putStrLn $ "    " ++ statInfoStr si
                printf "ship %d\n" shipId
                putStrLn "  asw: "
                pprInfo aInfo
                putStrLn "  evasion: "
                pprInfo eInfo
                putStrLn "  los: "
                pprInfo lInfo
                pure ()
            Nothing -> printf "ship %d: insufficient\n" shipId
    pure ()
processDump _ = do
    putStrLn "process-dump <dump file>"
    exitFailure

module Kantour.ShipStat.ProcessDump where

import Control.Arrow
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Coerce
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Data.String
import Data.Traversable
import GHC.Generics
import Kantour.ShipStat.Core
import System.Exit
import Text.Printf

data StatStruct a = StatStruct
  { _evasion :: a
  , _los :: a
  , _asw :: a
  }
  deriving (Eq, Functor)

newtype ShipStatRecord
  = ShipStatRecord (Int, (Int, StatStruct Int))
  deriving (Generic)

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
              (y : ys) -> guard (all (== y) ys) >> pure y
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

{-

key: shipId

value:

    - `Nothing` for insufficient amount of data (less than 2)

    - `Just x` otherwise

        - empty StatInfo means data given is inconsistent therefore exhausted search space
-}
type ShipStatInfoTable = IM.IntMap (Maybe (StatStruct [StatInfo]))

computeShipStatInfoTable :: FilePath -> IO ShipStatInfoTable
computeShipStatInfoTable fp = do
  (rawLines :: [Maybe ShipStatRecord]) <-
    fmap decode . LBSC.lines <$> LBS.readFile fp
  let lineCount = length rawLines
      xs = catMaybes rawLines
      validLineCount = length xs
  when (validLineCount /= lineCount) $
    putStrLn $ "invalid lines: " ++ show (lineCount - validLineCount)
  IM.map processShipLevelRecords <$> groupDumpInfoWithWarning xs

pprResult :: ShipStatInfoTable -> IO ()
pprResult tbl = void $
  flip IM.traverseWithKey tbl $ \shipId statInfoRowsM ->
    case statInfoRowsM of
      Just statInfoRows -> do
        let StatStruct {_evasion = eInfo, _asw = aInfo, _los = lInfo} = statInfoRows
            statInfoStr (StatInfo b d) = printf "base: %d, lv.99: %d" b (b + d)
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
      Nothing -> printf "ship %d: insufficient\n" shipId

newtype FinalReport = FinalReport ShipStatInfoTable deriving (Generic)

instance ToJSON FinalReport where
  toJSON (FinalReport shipTbl) = object $ reportShip <$> IM.toAscList shipTbl
    where
      reportShip (k, statResult) = fromString (show k) .= statResult'
        where
          statResult' = case statResult of
            Nothing -> "insufficient" :: Value
            Just st ->
              let convert :: StatInfo -> Value
                  convert (StatInfo baseSt stDiff) =
                    object ["base" .= baseSt, "max" .= (baseSt + stDiff)]
                  StatStruct
                    { _evasion = eInfo
                    , _asw = aInfo
                    , _los = lInfo
                    } = (fmap . fmap) convert st
               in object ["evasion" .= eInfo, "asw" .= aInfo, "los" .= lInfo]

processDump :: [String] -> IO ()
processDump [fp] = pprResult =<< computeShipStatInfoTable fp
processDump [fp, jsonOut] = do
  tbl <- computeShipStatInfoTable fp
  let encoded = encode (FinalReport tbl)
  LBS.writeFile jsonOut encoded
processDump _ = do
  putStrLn "process-dump <dump file>"
  putStrLn "process-dump <dump file> [out JSON file]"
  exitFailure

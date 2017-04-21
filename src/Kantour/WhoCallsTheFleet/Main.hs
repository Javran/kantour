{-# LANGUAGE OverloadedStrings, TypeFamilies, DuplicateRecordFields #-}
module Kantour.WhoCallsTheFleet.Main where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Aeson

import Data.Char
import Data.MonoTraversable
import Data.Semigroup
import Data.String
import Data.Word
import Control.Monad
import Data.Maybe
import Kantour.WhoCallsTheFleet.Types

import Kantour.WhoCallsTheFleet.Fetch (fetchShipsRaw)

w2c :: Word8 -> Char
w2c = chr . fromIntegral

splitLines :: LBS.ByteString -> [BS.ByteString]
splitLines = filter nonEmptyLine . BSC.split '\n' . LBS.toStrict
  where
    nonEmptyLine = oany (not . isSpace . w2c)

parseShip :: String -> Maybe Ship
parseShip xs = decode (fromString xs)

defaultMain :: IO ()
defaultMain = do
    raws <- splitLines <$> fetchShipsRaw
    let process :: BSC.ByteString -> IO ()
        process raw = do
            let result = eitherDecodeStrict' raw :: Either String Ship
            case result of
                Right m -> do
                    let mz = modernization (m :: Ship)
                    when (isNothing mz) $
                        BSC.putStrLn raw
                    pure ()
                Left msg -> do
                    putStrLn $ "parsing failed: " <> msg
                    BSC.putStrLn raw
            pure ()
    mapM_ process raws

data Equipment = Equipment
  { masterId :: Int
  , eqpRarity :: Int
  , eqpType :: Int
  , eqpName :: () -- TODO
  , eqpStat :: ()
  , eqpDismantle :: ()
  , eqpCraftable :: Bool
  , eqpImprovable :: Bool
  , eqpRankUpgradable :: Bool
  }


{-# LANGUAGE OverloadedStrings, TypeFamilies, DuplicateRecordFields #-}
module Kantour.WhoCallsTheFleet.Main where

import qualified Data.ByteString.Char8 as BSC
import Data.Aeson

import Data.Semigroup
import Data.String
import Control.Monad
import Data.Maybe
import Kantour.WhoCallsTheFleet.Types

import Kantour.WhoCallsTheFleet.Fetch (fetchShipsRaw)

parseShip :: String -> Maybe Ship
parseShip xs = decode (fromString xs)

defaultMain :: IO ()
defaultMain = do
    raws <- fetchShipsRaw
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


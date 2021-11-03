{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Kantour.FetchMasterData
  ( SubCmdFetchMasterData
  , fetchMasterData
  , loadFromSource
  )
where

import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap as IM
import Data.List
import Kantour.Core.KcData.Master.Root
import Kantour.Core.KcData.Master.Ship as Ship
import Kantour.Subcommand
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment
import System.Exit

data SubCmdFetchMasterData

{-
  TODO: the need of getting and parsing api_start2
  comes up so often that it might worth making into enviroment variables.
 -}

instance Subcommand SubCmdFetchMasterData where
  name _ = "FetchMasterData"
  main _ = defaultMain

{-
  TODO: subcommands can share Managers probably?
 -}

loadFromSource :: Manager -> String -> IO BSL.ByteString
loadFromSource mgr src
  | "http" `isPrefixOf` src = do
    req <- parseRequest src
    responseBody <$> httpLbs req mgr
  | otherwise = BSL.readFile src

fetchMasterData :: Manager -> String -> IO MasterRoot
fetchMasterData mgr src =
  loadFromSource mgr src >>= \rawJson ->
    case Aeson.eitherDecode @MasterRoot rawJson of
      Left msg -> die ("parse error: " <> msg)
      Right r -> pure r

{-
  TODO: do this later proper.
  TODO: we should probably have NFData on MasterRoot.

  Plan:
  - current reference impl is: https://github.com/Javran/subtender/blob/23d6f4d9e03cf81ffa0d070ed2bded46afbd1eae/src/poi/selectors.js#L67-L128
    and we want to re-implement it here
  - explore option on how to loop-break a circular chain (would be some combination of api_sortno, api_sort_id and api_id).
  - backport.

 -}
remodelChainExperiment :: MasterRoot -> IO ()
remodelChainExperiment MasterRoot {mstShip} = do
  let ships =
        IM.fromListWithKey
          (\k _ _ ->
             {-
               with Strict IntMap any error shall raise before the data structure is constructed.
              -}
             error $ "duplicated key: " <> show k)
          $ fmap (\s -> (Ship.shipId s, s)) allyShips
        where
          allyShips = filter ((<= 1500) . Ship.shipId) mstShip
  {-
    TODO: new algorithm:

    - screw it, let's do disjoint set. this ensures that all elements are accounted for.
    - also keep track of set of ship ids that have nothing pointing to them.
    - remove singletons
    - for each set, we want to find the base:
      + if one has no in-degree, it is the base.
      + otherwise we have to tie-break.
    - after base is determined, form the chain by BFS,
      (prefer BFS over DFS just in case there are branching remodels in the future)
      unattached ids should be alerted.

   -}
  _todo

defaultMain :: IO ()
defaultMain =
  getArgs >>= \case
    [fileOrUrlSrc] -> do
      mgr <- newManager tlsManagerSettings
      fetchMasterData mgr fileOrUrlSrc >>= remodelChainExperiment
    _ -> die "<subcmd> [http]<source>"

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Kantour.FetchMasterData
  ( SubCmdFetchMasterData
  , fetchMasterData
  , loadFromSource
  )
where

import Control.Monad
import Control.Monad.ST
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.UnionFind.ST as UF
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

-- cargoculting from myself: https://github.com/nominolo/union-find/issues/12#issuecomment-647090797
cluster :: [(UF.Point s Int, b)] -> ST s (IM.IntMap [b])
cluster ps =
  IM.map ($ []) . IM.fromListWith (flip (.))
    <$> mapM
      (\(uf, c) -> do
         cRep <- UF.descriptor =<< UF.repr uf
         pure (cRep, (c :)))
      ps

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
  let afterShipIdToMaybe :: Maybe T.Text -> Maybe Int
      afterShipIdToMaybe m = do
        r0 <- m
        [(v, "")] <- pure (reads $ T.unpack r0)
        guard $ v /= 0
        pure v
      ships =
        IM.fromListWithKey
          (\k _ _ ->
             {-
               with Strict IntMap any error shall raise before the data structure is constructed.
              -}
             error $ "duplicated key: " <> show k)
          $ fmap (\s -> (Ship.shipId s, s)) allyShips
        where
          allyShips = filter ((<= 1500) . Ship.shipId) mstShip
      (shipKs, shipVs) = unzip $ IM.toAscList ships
      remodelClusters :: IM.IntMap [Ship]
      remodelClusters = runST $ do
        pointsPre <- mapM (\s -> UF.fresh s >>= \p -> pure (s, p)) shipKs
        let points = IM.fromList pointsPre
        forM_ (IM.elems ships) $ \ship ->
          case afterShipIdToMaybe . Ship.aftershipid $ ship of
            Nothing -> pure ()
            Just afterId -> do
              UF.union (points IM.! Ship.shipId ship) (points IM.! afterId)
        cluster $ zip (fmap snd pointsPre) shipVs
      sortRemodel :: [Ship] -> [Ship]
      sortRemodel = sortOn (\s -> (rem (Ship.sortId s) 10, Ship.sortno s, Ship.shipId s))
      noInDegShips = IS.difference (IS.fromList shipKs) afterShipIds
        where
          afterShipIds =
            IS.fromList
              . mapMaybe (afterShipIdToMaybe . Ship.aftershipid)
              . IM.elems
              $ ships
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
  let handleShipCluster xs baseShipId = do
        putStrLn $ "TODO: base: " <> show baseShipId <> ", full set: " <> show (fmap Ship.shipId xs)
  forM_ (IM.toList remodelClusters) $ \(_k, vs) -> case vs of
    [v] -> putStrLn $ "Singleton: " <> show v
    _ ->
      case filter ((`IS.member` noInDegShips) . Ship.shipId) vs of
        [] -> do
          let sorted = sortRemodel vs
          let ppr xs =
                T.putStrLn $
                  T.unwords
                    ((\s ->
                        let ps x = T.pack (show x)
                         in Ship.name s <> "(" <> ps (Ship.shipId s) <> "," <> ps (rem (Ship.sortId s) 10) <> "," <> ps (fromJust $ Ship.sortno s) <> ")")
                       <$> xs)
          ppr sorted
          let sortIdLastDigits = fmap (\s -> rem (Ship.sortId s) 10) sorted
          unless (sortIdLastDigits == nub sortIdLastDigits) $
            T.putStrLn $ "> " <> T.unwords (fmap (\s -> T.pack $ show s) sortIdLastDigits)
          handleShipCluster vs (Ship.shipId $ head sorted)
        [baseShipId] ->
          handleShipCluster vs (Ship.shipId baseShipId)
        xs -> do
          putStrLn "Warning: multiple base ship?"
          print xs

defaultMain :: IO ()
defaultMain =
  getArgs >>= \case
    [fileOrUrlSrc] -> do
      mgr <- newManager tlsManagerSettings
      fetchMasterData mgr fileOrUrlSrc >>= remodelChainExperiment
    _ -> die "<subcmd> [http]<source>"

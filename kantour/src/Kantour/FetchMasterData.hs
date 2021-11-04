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
      remodelChains = runST $ do
        -- TODO: use vector.
        -- TODO: use IM.toList once rather than relying on IM.keys and IM.elems being consistent on ordering.
        pointsPre <- mapM (\s -> UF.fresh s >>= \p -> pure (s, p)) (IM.keys ships)
        let points = IM.fromList pointsPre
        forM_ (IM.elems ships) $ \ship ->
          case afterShipIdToMaybe . Ship.aftershipid $ ship of
            Nothing -> pure ()
            Just afterId -> do
              UF.union (points IM.! Ship.shipId ship) (points IM.! afterId)
        cluster $ zip (fmap snd pointsPre) (IM.elems ships)
      noInDegShips = IS.difference (IM.keysSet ships) afterShipIds
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
  forM_ (IM.toList remodelChains) $ \(_k, vs) -> do
    T.putStrLn $ T.unwords (fmap (\s -> Ship.name s <> "(" <> T.pack (show $ Ship.shipId s) <> ")") $ sortOn Ship.sortno vs)
  print (afterShipIdToMaybe . Ship.aftershipid <$> IM.elems ships)

defaultMain :: IO ()
defaultMain =
  getArgs >>= \case
    [fileOrUrlSrc] -> do
      mgr <- newManager tlsManagerSettings
      fetchMasterData mgr fileOrUrlSrc >>= remodelChainExperiment
    _ -> die "<subcmd> [http]<source>"

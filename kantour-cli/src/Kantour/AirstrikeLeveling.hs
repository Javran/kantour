module Kantour.AirstrikeLeveling (
  SubCmdAirstrikeLeveling,
) where

import Control.Monad.Except
import Control.Monad.Writer
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import qualified Data.Text as T
import Kantour.Core.KcData.Master.Fetch (fetchFromEnv)
import qualified Kantour.Core.KcData.Master.Org as Org
import qualified Kantour.Core.KcData.Master.Org.Ship as KcShip
import Kantour.Subcommand
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Shower
import System.Environment
import System.Exit
import Text.Printf
import Data.Coerce

{-
  Tools for calculating resource consumption on 5-2C node.

  TODO: impl

  ref: https://en.kancollewiki.net/Combat/Map_Mechanics_and_Nodes

  rsc consumption: fuel: 6%, ammo: 4%. rounded down but at least 1.
 -}

data SubCmdAirstrikeLeveling

instance Subcommand SubCmdAirstrikeLeveling where
  name _ = "Airstrike"
  main _ = defaultMain

type FuelAmmo = (Int, Int)

getShipInfo :: Org.Root -> Int -> Maybe (T.Text, FuelAmmo)
getShipInfo Org.Root {Org.ships} x = do
  KcShip.Ship
    { KcShip.name = n
    , KcShip.ours = Just KcShip.SOurs {KcShip.fuelAmmoMax}
    } <-
    ships IM.!? x
  pure (n, fuelAmmoMax)

airstrikeConsume :: FuelAmmo -> FuelAmmo -> Maybe (FuelAmmo, FuelAmmo) -- (after state, diff)
airstrikeConsume (fuelMax, ammoMax) (fuel, ammo) = do
  guard $ fuel > 0 && ammo > 0
  let consume :: Double -> Int -> Int -> (Int, Int)
      consume r mx x = (x - dx, dx)
        where
          dx = max 1 $ floor @Double @Int (r * fromIntegral mx)
      (fuel', df) = consume 0.06 fuelMax fuel
      (ammo', da) = consume 0.04 ammoMax ammo
  pure ((fuel', ammo'), (df, da))

fleetConsume :: [FuelAmmo] -> [FuelAmmo] -> Maybe ([FuelAmmo], FuelAmmo)
fleetConsume maxs xs = do
  ys <- zipWithM airstrikeConsume maxs xs
  let (df, da) = unzip $ fmap snd ys
  pure (fmap fst ys, (sum df, sum da))

calculateAndDisplay :: [Int] -> IO ()
calculateAndDisplay shipIds = do
  mgr <- newManager tlsManagerSettings
  mstRootD <- fetchFromEnv (Just mgr)
  case runExcept $ runWriterT (Org.fromDirect @Org.Root mstRootD) of
    Left msg -> die $ "master data error: " <> T.unpack msg
    Right (root, _) -> do
      let infos :: [(T.Text, FuelAmmo)]
          infos = fmap (fromJust . getShipInfo root) shipIds
          mx = fmap snd infos
      forM_ infos \(n, (x, y)) ->
        printf "%s: fuel: %d, ammo: %d\n" n x y

      fix
        ( \loop (cnt, st, tot :: (Sum Int, Sum Int)) -> do
            let cnt' = cnt + 1
            case fleetConsume mx st of
              Nothing -> pure ()
              Just (st' :: [FuelAmmo], diff :: FuelAmmo) -> do
                let tot' = tot <> coerce diff
                -- TODO: wrong result
                printf "%d: %s\n" cnt' (show $ coerce @_ @(Int, Int) tot')
                loop (cnt', st', tot')
        )
        (0 :: Int, mx, (0, 0))

defaultMain :: IO ()
defaultMain =
  getArgs
    >>= \case
      [raw] -> case eitherDecode' @[Int] (BSLC.pack raw) of
        Left msg -> die $ "decode error: " <> msg
        Right xs -> calculateAndDisplay xs
      _ -> die "<prog> '[ship ids, ...]'"

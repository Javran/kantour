module Kantour.AirstrikeLeveling
  ( SubCmdAirstrikeLeveling
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Function
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Kantour.Core.KcData.Master.Fetch (fetchFromEnv)
import qualified Kantour.Core.KcData.Master.Org as Org
import qualified Kantour.Core.KcData.Master.Org.Ship as KcShip
import Kantour.Subcommand
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment
import System.Exit
import Text.Printf

{-
  Tools for calculating resource consumption on 5-2C node.

  TODO: impl

  ref: https://en.kancollewiki.net/Combat/Map_Mechanics_and_Nodes

  rsc consumption: fuel: 6%, ammo: 4%. rounded down but at least 1.

  also:

  e.prototype.getFuelForSupply = function () {
    var bd = this.fuelMax - this.fuelNow
    if (0 < (bd = Math.max(0, bd)) && this.isMarriage()) {
      bd = Math.floor(0.85 * bd)
      bd = Math.max(bd, 1)
    }
    return bd
  }
  e.prototype.getAmmoForSupply = function () {
    var be = this.ammoMax - this.ammoNow
    if (0 < (be = Math.max(0, be)) && this.isMarriage()) {
      be = Math.floor(0.85 * be)
      be = Math.max(be, 1)
    }
    return be
  }

obtain input:

(() => {
  const {info} = getStore()
  const {ships, fleets} = info
  const xs = fleets[0].api_ship

  console.log(JSON.stringify(xs.map(x => {
    const {api_lv: lv, api_ship_id: id} = ships[x]
    return [id, lv]
  })))
})()

example input looks like:

> [[931,54],[285,135],[925,72],[185,43],[73,38],[372,45]]

 -}

data SubCmdAirstrikeLeveling

instance Subcommand SubCmdAirstrikeLeveling where
  name _ = "Airstrike"
  main _ = defaultMain

type FuelAmmo = (Int, Int)

getShipInfo :: Org.Root -> (Int, Int) -> Maybe (T.Text, (Bool, FuelAmmo))
getShipInfo Org.Root {Org.ships} (x, lvl) = do
  KcShip.Ship
    { KcShip.name = n
    , KcShip.ours = Just KcShip.SOurs {KcShip.fuelAmmoMax}
    } <-
    ships IM.!? x
  pure (n, (lvl >= 100, fuelAmmoMax))

airstrikeConsume :: (Bool, FuelAmmo) -> FuelAmmo -> Maybe (FuelAmmo, FuelAmmo) -- (after state, diff)
airstrikeConsume (_mar, (fuelMax, ammoMax)) (fuel, ammo) = do
  guard $ fuel > 0 && ammo > 0
  let consume :: Double -> Int -> Int -> (Int, Int)
      consume r mx x = (x - cost, cost)
        where
          cost = max 1 $ floor (fromIntegral mx * r)
      (fuel', df) = consume 0.06 fuelMax fuel
      (ammo', da) = consume 0.04 ammoMax ammo
  pure ((fuel', ammo'), (df, da))

fleetConsume :: [(Bool, FuelAmmo)] -> [FuelAmmo] -> Maybe ([FuelAmmo], FuelAmmo)
fleetConsume maxs xs = do
  ys <- zipWithM airstrikeConsume maxs xs
  let (df, da) = unzip $ fmap snd ys
  pure (fmap fst ys, (sum df, sum da))

calculateAndDisplay :: [(Int, Int)] -> IO ()
calculateAndDisplay ships = do
  mgr <- newManager tlsManagerSettings
  mstRootD <- fetchFromEnv (Just mgr)
  case runExcept $ runWriterT (Org.fromDirect @Org.Root mstRootD) of
    Left msg -> die $ "master data error: " <> T.unpack msg
    Right (root, _) -> do
      let infos :: [(T.Text, (Bool, FuelAmmo))]
          infos = fmap (fromJust . getShipInfo root) ships
          mx = fmap snd infos
      forM_ infos \(n, (mar, (x, y))) ->
        printf "%s: marriage: %c, fuel: %d, ammo: %d\n" n (if mar then 'y' else 'n') x y

      fix
        ( \loop (cnt, st) -> do
            let cnt' = cnt + 1
            case fleetConsume mx st of
              Nothing -> pure ()
              Just (st' :: [FuelAmmo], _) -> do
                let (Sum fuelCost, Sum ammoCost) = mconcat $ zipWith computeCost infos st'
                    computeCost :: (T.Text, (Bool, FuelAmmo)) -> FuelAmmo -> (Sum Int, Sum Int)
                    computeCost (_, (mar, (fuMax, amMax))) (fu, am) = (Sum $ f fuMax fu, Sum $ f amMax am)
                      where
                        f cap x =
                          if mar
                            then max 1 $ floor @Double (0.85 * fromIntegral (cap - x))
                            else cap - x

                let ave v = fromIntegral v / (fromIntegral cnt' :: Double)
                printf "%d: %s (%.2f, %.2f))\n" cnt' (show (fuelCost, ammoCost)) (ave fuelCost) (ave ammoCost)
                loop (cnt', st')
        )
        (0 :: Int, fmap snd mx)

defaultMain :: IO ()
defaultMain =
  getArgs
    >>= \case
      [raw] -> case eitherDecode' @[(Int, Int)] (BSLC.pack raw) of
        Left msg -> die $ "decode error: " <> msg
        Right xs -> calculateAndDisplay xs
      _ -> die "<prog> '[ship ids, ...]'"

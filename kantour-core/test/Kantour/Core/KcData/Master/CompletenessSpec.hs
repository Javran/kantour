module Kantour.Core.KcData.Master.CompletenessSpec (
  spec,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Picker
import qualified Data.ByteString.Lazy as BSL
import Data.Containers.ListUtils
import qualified Data.Map.Strict as M
import Data.Proxy
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import GHC.Generics
import Kantour.Core.DataFiles
import Kantour.Core.KcData.Master.Bgm
import Kantour.Core.KcData.Master.Common
import Kantour.Core.KcData.Master.Const
import Kantour.Core.KcData.Master.EquipExslotShip
import Kantour.Core.KcData.Master.EquipShip
import Kantour.Core.KcData.Master.Furniture
import Kantour.Core.KcData.Master.Furnituregraph
import Kantour.Core.KcData.Master.ItemShop
import Kantour.Core.KcData.Master.Maparea
import Kantour.Core.KcData.Master.Mapbgm
import Kantour.Core.KcData.Master.Mapinfo
import Kantour.Core.KcData.Master.Mission
import Kantour.Core.KcData.Master.Payitem
import Kantour.Core.KcData.Master.Root
import Kantour.Core.KcData.Master.Ship
import Kantour.Core.KcData.Master.Shipgraph
import Kantour.Core.KcData.Master.Shipupgrade
import Kantour.Core.KcData.Master.Slotitem
import Kantour.Core.KcData.Master.SlotitemEquiptype
import Kantour.Core.KcData.Master.Stype
import Kantour.Core.KcData.Master.Useitem
import Test.Hspec

{-
  This module verifies that all fields in source master data
  are properly parsed and signal unknown fields if any.
 -}

loadMaster :: IO Value
loadMaster =
  loadDataFile "data/api_start2.json.xz" >>= \raw ->
    case eitherDecode raw of
      Left msg -> fail $ "Cannot parse data: " <> msg
      Right v -> pure v

spec :: Spec
spec = describe "Completeness" $
  beforeAll loadMaster $ do
    let mkTest ::
          forall p a d f.
          ( FromJSON a
          , HasKnownFields a
          , Generic a
          , Rep a ~ M1 D d f
          , Datatype d
          ) =>
          p a ->
          (Value -> [Value]) ->
          SpecWith Value
        mkTest _ty selector =
          let dName = datatypeName (from @a undefined)
           in specify dName $ \rawMst -> do
                let xs = selector rawMst
                    ys :: [CollectExtra a]
                    ys = fmap (getResult . fromJSON @(CollectExtra a)) xs
                      where
                        getResult = \case
                          Error msg -> error msg
                          Success v -> v
                    unknownFields = M.fromListWith (<>) $ do
                      CollectExtra {ceExtra} <- ys
                      (k, v) <- ceExtra
                      pure (k, [v])
                unless (unknownFields == M.empty) $ do
                  liftIO $ do
                    putStrLn $ "Unknown fields detected for " <> dName <> ":"
                    forM_ (M.toAscList unknownFields) $ \(k, vsPre) -> do
                      let vs = take 5 $ nubOrd $ fmap encode vsPre
                      T.putStrLn $ "  Samples for " <> k <> ":"
                      forM_ vs $ \v ->
                        T.putStrLn $ "  - " <> decodeUtf8 (BSL.toStrict v)
                  pendingWith $
                    "Unknown fields: " <> unwords (T.unpack <$> M.keys unknownFields)
    let sel x v = v |-- [x]
        sel' x v = [sel x v]

    mkTest (Proxy @Slotitem) $ sel "api_mst_slotitem"
    mkTest (Proxy @Shipgraph) $ sel "api_mst_shipgraph"
    mkTest (Proxy @Ship) $ sel "api_mst_ship"
    mkTest (Proxy @Bgm) $ sel "api_mst_bgm"
    mkTest (Proxy @EquipExslotShip) $ sel "api_mst_equip_exslot_ship"
    mkTest (Proxy @EquipShip) $ sel "api_mst_equip_ship"
    mkTest (Proxy @Furniture) $ sel "api_mst_furniture"
    mkTest (Proxy @Furnituregraph) $ sel "api_mst_furnituregraph"
    mkTest (Proxy @Maparea) $ sel "api_mst_maparea"
    mkTest (Proxy @Mapbgm) $ sel "api_mst_mapbgm"
    mkTest (Proxy @Mapinfo) $ sel "api_mst_mapinfo"
    mkTest (Proxy @Mission) $ sel "api_mst_mission"
    mkTest (Proxy @Payitem) $ sel "api_mst_payitem"
    mkTest (Proxy @Shipupgrade) $ sel "api_mst_shipupgrade"
    mkTest (Proxy @SlotitemEquiptype) $ sel "api_mst_slotitem_equiptype"
    mkTest (Proxy @Stype) $ sel "api_mst_stype"
    mkTest (Proxy @Useitem) $ sel "api_mst_useitem"

    mkTest (Proxy @Const) $ sel' "api_mst_const"
    mkTest (Proxy @ItemShop) $ sel' "api_mst_item_shop"
    mkTest (Proxy @MasterRoot) pure

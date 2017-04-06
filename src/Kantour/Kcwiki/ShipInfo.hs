module Kantour.Kcwiki.ShipInfo where

-- import this as qualified.

-- https://zh.kcwiki.moe/wiki/%E6%A8%A1%E5%9D%97:%E8%88%B0%E5%A8%98%E6%95%B0%E6%8D%AE

data ShipInfo = ShipInfo
  { kcwikiId :: String
    -- "图鉴号" -- ?
  , masterId :: Int
  , nameJP :: String
  , kana :: String
  , nameCN :: String
  , stype :: Int
  , shipClass :: ShipClass
  , stats :: ShipStats
  , equipmentInfo :: ShipEquipmentInfo
  , obtain :: ShipObtainMethod
  , consumption :: ShipConsumption
  , asFodder :: ShipAsFodder
  , dismantle :: ShipDismantle
  , remodel :: ()
  , eShi :: String
  , seiyu :: String
  }

data ShipClass = ShipClass
  { sClassName :: String
  , sDesination :: Int
  }

type Ranged a = (a,a)

data ShipStats = ShipStats
  { hitPoints :: Ranged Int
  , firePower :: Ranged Int
  , torpedo :: Ranged Int
  , antiAir :: Ranged Int
  , armor :: Ranged Int
  , antiSub :: Ranged Int
  , evasion :: Ranged Int
  , los :: Ranged Int
  , luck :: Ranged Int
  , speed :: Int
  , range :: Int
  , rarity :: Int
  }

data ShipEquipmentInfo = ShipEquipmentInfo
  { slotCount :: Int
  , slots :: [Int]
  , initialEquipments :: [Int]
  }

data ShipObtainMethod = ShipObtainMethod
  { isDroppable :: Bool
  , isThroughRemodel :: Bool
  , isBuildable :: Bool
  , buildTime :: Int
  }

data ShipConsumption = ShipConsumption
  { maxFuel :: Int
  , maxAmmo :: Int
  }

data ShipAsFodder = ShipAsFodder
  { fodFirePower :: Int
  , fodTorpedo :: Int
  , fodAntiAir :: Int
  , fodArmor :: Int
  }

data ShipDismantle = ShipDismantle
  { sdFuel :: Int
  , sdAmmo :: Int
  , sdSteel :: Int
  , sdBauxite :: Int
  }

data ShipRemodel = ShipRemodel
  { rmdLevel :: Int
  , rmdAmmo :: Int
  , rmdSteel :: Int
  , rmdPrevId :: Maybe String
  , rmdNextId :: Maybe String
  }

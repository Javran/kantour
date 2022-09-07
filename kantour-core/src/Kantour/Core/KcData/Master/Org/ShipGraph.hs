{-# LANGUAGE DuplicateRecordFields #-}

module Kantour.Core.KcData.Master.Org.ShipGraph (
  SGOurs (..),
  ShipGraph (..),
) where

import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import qualified Kantour.Core.KcData.Master.Direct.Shipgraph as D
import Kantour.Core.KcData.Master.Org.Common

type Coord = (Int, Int)
type DnPair = (Coord, Coord)

-- fields found only in Our side.
data SGOurs = SGOurs
  { sortNo :: Int
  , boko :: DnPair
  , ensyuEN :: Coord
  , ensyuF :: DnPair
  , kaisyu :: DnPair
  , kaizo :: DnPair
  , sgMap :: DnPair
  , pa :: Coord
  , pab :: Coord
  , wedAB :: (Coord, Coord)
  }
  deriving (Generic, Show)

instance NFData SGOurs

data ShipGraph = ShipGraph
  { kcId :: Int
  , fileName :: T.Text
  , version :: (Int, Int, Int)
  , battle :: Maybe DnPair
  , ours :: Maybe SGOurs
  }
  deriving (Generic, Show)

instance NFData ShipGraph

instance FromDirect ShipGraph where
  type Source ShipGraph = D.Shipgraph

  fromDirect
    D.Shipgraph
      { kcId
      , filename = fileName
      , version = verPre
      , battleD
      , battleN
      , sortno
      , bokoD
      , bokoN
      , ensyueN
      , ensyufD
      , ensyufN
      , kaisyuD
      , kaisyuN
      , kaizoD
      , kaizoN
      , mapD
      , mapN
      , pa = paPre
      , pab = pabPre
      , weda
      , wedb
      } = do
      let tup2 = \case
            Just [a, b] -> pure (a, b)
            _ -> Nothing
          dn d n = do
            d' <- tup2 d
            n' <- tup2 n
            pure (d', n')

      version <- case mapMaybe textToInt $ NE.toList verPre of
        [a, b, c] -> pure (a, b, c)
        _ -> illformed "version"
      let side
            | kcId <= 1500 = Our
            | kcId < 5000 = Abyssal
            | otherwise = Seasonal
          battle = dn battleD battleN
          ours = do
            sortNo <- sortno
            boko <- dn bokoD bokoN
            ensyuEN <- tup2 ensyueN
            ensyuF <- dn ensyufD ensyufN
            kaisyu <- dn kaisyuD kaisyuN
            kaizo <- dn kaizoD kaizoN
            sgMap <- dn mapD mapN
            pa <- tup2 paPre
            pab <- tup2 pabPre
            wedAB <- (,) <$> tup2 weda <*> tup2 wedb
            pure
              SGOurs
                { sortNo
                , boko
                , ensyuEN
                , ensyuF
                , kaisyu
                , kaizo
                , sgMap
                , pa
                , pab
                , wedAB
                }
      case (side, battle, ours) of
        (Our, Just _, Just _) -> pure ()
        (Our, _, _) -> illformed "our, battle or our attribs"
        (Abyssal, Just _, Nothing) -> pure ()
        (Abyssal, _, _) -> illformed "abyssal, battle or our attribs"
        (Seasonal, Nothing, Nothing) -> pure ()
        (Seasonal, _, _) -> illformed "seasonal, battle or our attribs"
      pure
        ShipGraph
          { kcId
          , fileName
          , version
          , battle
          , ours
          }

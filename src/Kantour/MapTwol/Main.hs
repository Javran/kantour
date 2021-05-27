{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Kantour.MapTwol.Main where

{-
  TODO: status: under construction.
 -}

import Control.Arrow
import Control.Monad
import Data.Aeson
import Data.Coerce
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Image as Img
import qualified Kantour.KcData.Map.Background as Bg
import qualified Kantour.KcData.Map.Enemy as Enemy
import qualified Kantour.KcData.Map.Image as KcImage
import qualified Kantour.KcData.Map.Info as KcInfo
import qualified Kantour.KcData.Map.Spot as Spot
import qualified Kantour.KcData.Map.Sprite as Sprite
import qualified Kantour.KcData.Map.Xywh as Xywh
import Kantour.MapTwol.Superimpose
import Kantour.Subcommand
import System.Environment
import System.FilePath.Posix
import Text.Printf

data SubCmdMapTwol

instance Subcommand SubCmdMapTwol where
  name _ = "MapTwol"
  main _ = defaultMain

-- SpotPointImage._getTexture
spotPointMap :: IM.IntMap Int
spotPointMap =
  IM.fromList
    [ (-1, 55)
    , (1, 48)
    , (2, 51)
    , (6, 51)
    , (3, 53)
    , (4, 54)
    , (5, 42)
    , (7, 40)
    , (8, 41)
    , (9, 52)
    , (10, 39)
    , (11, 56)
    , (12, 57)
    , (13, 17)
    , (-2, 50)
    , (-3, 47)
    , (14, 48)
    ]

defaultMain :: IO ()
defaultMain =
  getArgs >>= \case
    ["dev-combine", srcFileList, dstFile] -> do
      {-
        Accepts a list of JSON file paths, read them and combine
        it into an Array.

        The purpose of this is to see if it's possible to infer types from
        multiple samples using https://github.com/jvilk/MakeTypes.
       -}
      fps <- lines <$> readFile srcFileList
      vals <- forM fps $ \fp ->
        eitherDecodeFileStrict @Value fp >>= \case
          Left msg -> error $ "failed to parse " <> fp <> ": " <> msg
          Right v -> pure v
      let resultObj = Array $ Vec.fromList vals
      encodeFile dstFile resultObj
    ["parse-all", srcFileList] -> do
      fps <- lines <$> readFile srcFileList
      _vals <- forM fps $ \fp ->
        eitherDecodeFileStrict @KcInfo.Info fp >>= \case
          Left msg -> error $ "failed to parse " <> fp <> ": " <> msg
          Right v -> pure v
      -- let Just xs = (traverse . traverse) (Just ) $ fmap (: []) vals
      putStrLn $ "all " <> show (length fps) <> " files parsed."
    -- mapM_ print xs
    [ "extract-sprite"
      , srcPrefix {- e.g. /some/local/resource/path/kcs2/resources/map/004/05_image -}
      , dstDir {- assume existing -}
      ] -> do
        {-
          Separate sprites into a directory.
         -}
        imgs <- extractSpriteFromFilePrefix srcPrefix
        forM_ (HM.toList imgs) $ \(fn, (_, spImg)) -> do
          Img.writeImageExact Img.PNG [] (dstDir </> T.unpack fn) spImg
        putStrLn $ show (HM.size imgs) <> " files written."
    [ "extract-map"
      , kcs2Prefix {- e.g. /some/local/resource/path/kcs2 (remove final '/' if any) -}
      , worldRaw {- e.g. '48' -}
      , areaRaw {- e.g. '7' -}
      , dstDir {- assume existing -}
      ] -> do
        let srcPrefix :: FilePath
            srcPrefix =
              kcs2Prefix
                </> printf
                  "resources/map/%03d/%02d"
                  (read worldRaw :: Int)
                  (read areaRaw :: Int)
        Right mapInfo <- eitherDecodeFileStrict @KcInfo.Info (srcPrefix <> "_info.json")
        imgsPre <- extractSpriteFromFilePrefix (srcPrefix <> "_image")
        mapMainPre <- extractSpriteFromFilePrefix (kcs2Prefix </> "img/map/map_main")
        let (prefix, imgs) = stripSpriteKeyPrefix imgsPre
            mapMainImgs = IM.fromList . (fmap . first) toInt . HM.toList $ mapMainPre
              where
                magic = "map_main_"
                toInt raw =
                  if magic `T.isPrefixOf` raw
                    then read . T.unpack $ T.drop (T.length magic) raw
                    else error "invalid map_main key"
        putStrLn $ "Prefix " <> show prefix <> " removed from sprite file names."
        let containRed = False
            spotToImgs :: Spot.Spot -> [(Xywh.Xy Int, Img.Image Img.VS Img.RGBA Double)]
            spotToImgs
              Spot.Spot
                { Spot.x = spX
                , Spot.y = spY
                , Spot.offsets = ms
                } =
                fmap
                  (\(k, (Xywh.Xy (dx, dy))) ->
                     let (Sprite.Sprite {Sprite.sourceSize = Xywh.Wh (w, h)}, img) =
                           mapMainImgs IM.! (spotPointMap IM.! (read $ T.unpack k))
                      in ( -- Note: this appears to work but I'm not entirely sure if it's correct.
                           Xywh.Xy (spX + dx - w `div` 2, spY + dy - h `div` 2)
                         , img
                         ))
                  $ maybe [] HM.toList $ ms
            extraSpotImgs :: [(Xywh.Xy Int, Img.Image Img.VS Img.RGBA Double)]
            extraSpotImgs = foldMap spotToImgs $ KcInfo.spots mapInfo
            bgs =
              concatMap
                (\b ->
                   [ snd $ imgs HM.! Bg.img b
                   | Bg.name b /= Just "red" || containRed
                   ])
                . Vec.toList
                . fromJust
                . KcInfo.bg
                $ mapInfo
            combinedBgPre =
              foldl1 (\acc i -> superimpose' (0, 0) i acc) bgs
            combinedBg = foldl (\acc (Xywh.Xy (x, y), i) -> superimpose' (y, x) i acc) combinedBgPre extraSpotImgs
            ems :: [Enemy.Enemy]
            ems = maybe [] Vec.toList (KcInfo.enemies mapInfo)
            withEnemies =
              foldl
                (\acc e ->
                   let Enemy.Enemy {Enemy.x, Enemy.y, Enemy.img = iK} = e
                       (_, i) = imgs HM.! iK
                    in superimpose' (y, x) i acc)
                combinedBg
                ems
        Img.writeImageExact Img.PNG [] (dstDir </> "gen_background.png") combinedBg
        Img.writeImageExact Img.PNG [] (dstDir </> "gen_with_enemies.png") withEnemies
    _ -> do
      putStrLn "<file list> <target file>"

stripSpriteKeyPrefix :: HM.HashMap T.Text b -> (T.Text, HM.HashMap T.Text b)
stripSpriteKeyPrefix m =
  (commonPrefix, HM.fromList . (fmap . first) (T.drop prefixLen) $ HM.toList m)
  where
    prefixLen = T.length commonPrefix
    commonPrefix = T.take (i + 1) k
      where
        -- assuming this map is always non-empty
        (k, _) = head (HM.toList m)
        Just i = T.findIndex (== '_') k

extractSpriteFromFilePrefix
  :: FilePath -> IO (HM.HashMap T.Text (Sprite.Sprite, Img.Image Img.VS Img.RGBA Double))
extractSpriteFromFilePrefix srcPrefix = do
  Right img <-
    Img.readImageExact
      @(Img.Image Img.VS Img.RGBA Double)
      Img.PNG
      (srcPrefix <> ".png")
  Right meta <- eitherDecodeFileStrict @KcImage.Image (srcPrefix <> ".json")
  let sprites = KcImage.frames meta
      imgs = extractSprite img sprites
  pure imgs

extractSprite
  :: Img.Array arr cs e
  => Img.Image arr cs e
  -> HM.HashMap k Sprite.Sprite
  -> HM.HashMap k (Sprite.Sprite, Img.Image arr cs e)
extractSprite img = HM.map (id &&& convert)
  where
    convert sp = Img.crop (y, x) (h, w) img
      where
        ((x, y), (w, h)) = coerce (Sprite.frame sp)

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
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Image as Img
import qualified Kantour.KcData.Map.Background as Bg
import qualified Kantour.KcData.Map.Enemy as Enemy
import qualified Kantour.KcData.Map.Image as KcImage
import qualified Kantour.KcData.Map.Info as KcInfo
import qualified Kantour.KcData.Map.Sprite as Sprite
import qualified Kantour.KcData.Map.Xywh as Xywh
import Kantour.MapTwol.Superimpose
import Kantour.Subcommand
import System.Environment
import System.FilePath.Posix

data SubCmdMapTwol

instance Subcommand SubCmdMapTwol where
  name _ = "MapTwol"
  main _ = defaultMain

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
      , srcPrefix {- e.g. /some/local/resource/path/kcs2/resources/map/004/05 -}
      , dstDir {- assume existing -}
      ] -> do
        Right img <-
          Img.readImageExact
            @(Img.Image Img.VS Img.RGBA Double)
            Img.PNG
            (srcPrefix <> "_image.png")
        Right meta <- eitherDecodeFileStrict @KcImage.Image (srcPrefix <> "_image.json")
        Right mapInfo <- eitherDecodeFileStrict @KcInfo.Info (srcPrefix <> "_info.json")
        let (prefix, sprites) = stripSpriteKeyPrefix (KcImage.frames meta)
            imgs = extractSprite img sprites
        putStrLn $ "Prefix " <> show prefix <> " removed from sprite file names."
        forM_ (HM.toList imgs) $ \(fn, (_, spImg)) -> do
          Img.writeImageExact Img.PNG [] (dstDir </> T.unpack fn) spImg
        putStrLn $ show (HM.size imgs) <> " files written."
        let containRed = False
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
            combinedBg =
              foldl1 (\acc i -> superimpose' (0, 0) i acc) bgs
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

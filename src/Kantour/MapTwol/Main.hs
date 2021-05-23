{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Kantour.MapTwol.Main where

{-
  TODO: status: under construction.
 -}

import Control.Monad
import Data.Aeson
import Data.Coerce
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Image as Img
import qualified Kantour.KcData.Map.Image as KcImage
import qualified Kantour.KcData.Map.Sprite as Sprite
import qualified Kantour.KcData.Map.Xywh as Xywh
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
      (vals :: [KcImage.Image]) <- forM fps $ \fp ->
        eitherDecodeFileStrict @KcImage.Image fp >>= \case
          Left msg -> error $ "failed to parse " <> fp <> ": " <> msg
          Right v -> pure v
      let Just xs = (traverse . traverse) (Just . HM.elems . KcImage.frames) $ fmap (: []) vals
      putStrLn $ "all " <> show (length fps) <> " files parsed."
      mapM_ print xs
    [ "extract-sprite"
      , srcPrefix {- the part without ".png" or ".json" -}
      , dstDir {- assume existing -}
      ] -> do
        Right img <- Img.readImageExact @(Img.Image Img.VS Img.RGBA Img.Word8) Img.PNG (srcPrefix <> ".png")
        Right meta <- eitherDecodeFileStrict @KcImage.Image (srcPrefix <> ".json")
        let imgs = extractSprite img (KcImage.frames meta)
        forM_ (HM.toList imgs) $ \(fn, spImg) -> do
          Img.writeImageExact Img.PNG [] (dstDir </> T.unpack fn) spImg
    _ -> do
      putStrLn "<file list> <target file>"

extractSprite
  :: Img.Array arr cs e
  => Img.Image arr cs e
  -> HM.HashMap k Sprite.Sprite
  -> HM.HashMap k (Img.Image arr cs e)
extractSprite img = HM.map convert
  where
    convert sp = Img.crop (y, x) (h, w) img
      where
        ((x, y), (w, h)) = coerce (Sprite.frame sp)

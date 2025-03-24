{-
  This module deals with cropping individual images from spritesmith.

  For now, the purpose is not to deal with every possible situation
  but to be sufficient for kc assets.

 -}

module Kantour.Sprite.Spritesmith
  ( Xy (..)
  , Wh (..)
  , FrameInfo (..)
  , SpriteFrames (..)
  , FileMeta (..)
  , FileInfo (..)
  , extractImage
  , loadSpritesmith
  , outputImages
  )
where

import Control.Monad
import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Tuple
import GHC.Generics
import qualified Graphics.Image as Img
import System.Exit (die)
import System.FilePath

{-
  Example resource:
  - /kcs2/img/common/common_itemicons.json?version=4.5.3.0
  - /kcs2/img/common/common_itemicons.png?version=4.5.3.0

  somehow we need an path for output and we can download all stuff to there.

  also need a dedicate module for https://github.com/twolfson/spritesmith,
  I think we can write one just for PNG format.

  structure of this json file:

  - frames: an object
    - keys are file names, values are objects
      - frame:
        - x,y,w,h
        - rotated: false
        - trimmed: false
        - spriteSourceSize: {x,y,w,h}
        - sourceSize: {w,h}

  example:
  - "common_itemicons_id_75",FrameInfo {fiCoord = (480,80), fiSize = (75,75)}
  - item: 新型砲熕兵装資材
  - examined by gimp (top-left corner is (1,1)), the corresponding icon goes from (481,81) to (555,155).
 -}

newtype Xy a = Xy {getXy :: (a, a)}
  deriving newtype (Eq, Ord)
  deriving stock (Show, Functor)

instance FromJSON a => FromJSON (Xy a) where
  parseJSON = withObject "Xy" $ \v -> do
    x <- v .: "x"
    y <- v .: "y"
    pure $ Xy (x, y)

newtype Wh a = Wh {getWh :: (a, a)}
  deriving newtype (Eq, Ord)
  deriving stock (Show, Functor)

instance FromJSON a => FromJSON (Wh a) where
  parseJSON = withObject "Wh" $ \v -> do
    w <- v .: "w"
    h <- v .: "h"
    pure $ Wh (w, h)

data FrameInfo = FrameInfo
  { fiCoord :: Xy Int
  , fiSize :: Wh Int
  }
  deriving (Generic, Show)

instance FromJSON FrameInfo where
  parseJSON = withObject "FrameInfo" $ \v -> do
    rotated <- v .: "rotated"
    when rotated $
      fail "rotated shouldn't be False"
    trimmed <- v .: "trimmed"
    when trimmed $
      fail "trimmed shouldn't be False"
    frame <- v .: "frame"

    xy <- parseJSON frame
    wh <- parseJSON frame
    pure $ FrameInfo xy wh

newtype SpriteFrames
  = SpriteFrames (M.Map T.Text FrameInfo)
  deriving (Generic, Show)

instance FromJSON SpriteFrames

data FileMeta = FileMeta
  { fmSize :: Wh Int
  }
  deriving (Show)

instance FromJSON FileMeta where
  parseJSON = withObject "FileMeta" $ \v -> do
    format <- v .: "format"
    when (format /= "RGBA8888") $
      fail $
        "unexpected format: " <> T.unpack format
    sizeObj <- v .: "size"
    FileMeta <$> parseJSON sizeObj

newtype FileInfo
  = FileInfo (SpriteFrames, FileMeta)
  deriving (Show)

instance FromJSON FileInfo where
  parseJSON = withObject "FileInfo" $ \v -> do
    sf <- v .: "frames"
    fm <- v .: "meta"
    pure $ FileInfo (sf, fm)

extractImage :: Image -> FrameInfo -> Image
extractImage img FrameInfo {fiCoord, fiSize} =
  Img.crop (swap $ getXy fiCoord) (swap $ getWh fiSize) img

type Image = Img.Image Img.VS Img.RGBA Img.Word8

loadSpritesmith :: FilePath -> FilePath -> IO (M.Map T.Text Image)
loadSpritesmith jsonFile pngFile = do
  FileInfo (SpriteFrames sf, FileMeta (Wh sz)) <-
    eitherDecodeFileStrict jsonFile >>= \case
      Left err -> die $ "JSON parse error: " <> err
      Right v -> pure v

  mapM_ print (M.toAscList sf)
  img <- Img.readImageExact' Img.PNG pngFile
  -- note that here hip dimension is represented as (h,w), rather than (w,h).
  let (imgH, imgW) = Img.dims img
  when (sz /= (imgW, imgH)) $
    error $
      "image size mismatch: " <> show (sz, (imgW, imgH))
  putStrLn $ "width: " <> show imgW <> ", height: " <> show imgH
  pure $ M.map (extractImage img) sf

outputImages :: FilePath -> M.Map T.Text Image -> IO ()
outputImages outputDir = mapM_ outputImage . M.toList
  where
    outputImage :: (T.Text, Image) -> IO ()
    outputImage (name, img) =
      Img.writeImageExact Img.PNG [] outputFileName img
      where
        outputFileName = outputDir </> T.unpack name <.> "png"

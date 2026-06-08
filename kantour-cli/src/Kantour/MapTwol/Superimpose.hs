{-# LANGUAGE BangPatterns #-}

{-
  For whatever reason fourmolu is upset about BangPatterns despite that it's in
  default extensions.
 -}

module Kantour.MapTwol.Superimpose where

import Data.Massiv.Array
import Kantour.Image

-- https://github.com/lehins/hip/issues/33
superimpose' ::
  (Fractional e, Ord e, Storable e) =>
  -- | @(i, j)@ starting index from within a source image.
  (Int, Int) ->
  -- | Image to be positioned above the source image.
  Array S Ix2 (RGBA e) ->
  -- | Source image.
  Array S Ix2 (RGBA e) ->
  Array S Ix2 (RGBA e)
superimpose' (!i0, !j0) !imgA !imgB =
  computeAs S $ makeArrayR D Seq (size imgB) $ \(Ix2 i j) ->
    let
      Sz (Ix2 m n) = size imgA
      i' = i - i0
      j' = j - j0
      old = imgB ! Ix2 i j
      new = imgA ! Ix2 i' j'
     in
      if i' >= 0 && j' >= 0 && i' < m && j' < n then overlayAlpha old new else old

-- https://en.wikipedia.org/wiki/Alpha_compositing
overlayAlpha ::
  (Ord e, Fractional e) =>
  RGBA e ->
  RGBA e ->
  RGBA e
overlayAlpha (PixelRGBA br bg bb ba) (PixelRGBA oR oG oB oA) =
  PixelRGBA (f br oR) (f bg oG) (f bb oB) ra
  where
    ra = oA + ba * (1 - oA)
    f b o =
      if ra == 0
        then 0
        else (o * oA + b * ba * (1 - oA)) / ra

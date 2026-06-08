{-# LANGUAGE BangPatterns #-}

{-
  For whatever reason fourmolu is upset about BangPatterns despite that it's in
  default extensions.
 -}

module Kantour.MapTwol.Superimpose where

import Data.Massiv.Array (Array, Ix2(..), Sz(..), size, (!), makeArrayR, computeAs, D(..), S(..), Comp(..))
import Data.Massiv.Array.IO (Pixel(..), Alpha, SRGB, Linearity(..))
import Foreign.Storable (Storable)

-- https://github.com/lehins/hip/issues/33
superimpose' ::
  (Fractional e, Ord e, Storable e) =>
  -- | @(i, j)@ starting index from within a source image.
  (Int, Int) ->
  -- | Image to be positioned above the source image.
  Array S Ix2 (Pixel (Alpha (SRGB NonLinear)) e) ->
  -- | Source image.
  Array S Ix2 (Pixel (Alpha (SRGB NonLinear)) e) ->
  Array S Ix2 (Pixel (Alpha (SRGB NonLinear)) e)
superimpose' (!i0, !j0) !imgA !imgB =
  computeAs S $ makeArrayR D Seq (size imgB) $ \(i :. j) ->
    let Sz (m :. n) = size imgA
        i' = i - i0
        j' = j - j0
        old = imgB ! (i :. j)
        new = imgA ! (i' :. j')
    in if i' >= 0 && j' >= 0 && i' < m && j' < n then overlayAlpha old new else old

-- https://en.wikipedia.org/wiki/Alpha_compositing
overlayAlpha ::
  (Ord e, Fractional e) =>
  Pixel (Alpha (SRGB NonLinear)) e ->
  Pixel (Alpha (SRGB NonLinear)) e ->
  Pixel (Alpha (SRGB NonLinear)) e
overlayAlpha (PixelRGBA br bg bb ba) (PixelRGBA or og ob oa) =
  PixelRGBA (f br or) (f bg og) (f bb ob) ra
  where
    ra = oa + ba * (1 - oa)
    f b o =
      if ra == 0
        then 0
        else (o * oa + b * ba * (1 - oa)) / ra

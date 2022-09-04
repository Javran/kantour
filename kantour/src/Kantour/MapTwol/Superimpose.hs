{-# LANGUAGE BangPatterns #-}

{-
  For whatever reason fourmolu is upset about BangPatterns despite that it's in
  default extensions.
 -}

module Kantour.MapTwol.Superimpose where

import Graphics.Image
import Graphics.Image.Interface

-- https://github.com/lehins/hip/issues/33
superimpose' ::
  ( Array arr RGBA e
  , AlphaSpace RGBA e
  , Fractional e
  , Ord e
  ) =>
  -- | @(i, j)@ starting index from within a source image.
  (Int, Int) ->
  -- | Image to be positioned above the source image.
  Image arr RGBA e ->
  -- | Source image.
  Image arr RGBA e ->
  Image arr RGBA e
superimpose' (!i0, !j0) !imgA !imgB = traverse2 imgB imgA const newPx
  where
    !(m, n) = dims imgA
    newPx getPxB getPxA (i, j) =
      let !(i', j') = (i - i0, j - j0)
          new = getPxA (i', j')
          old = getPxB (i, j)
       in if i' >= 0 && j' >= 0 && i' < m && j' < n then overlayAlpha old new else old

-- https://en.wikipedia.org/wiki/Alpha_compositing
overlayAlpha ::
  (Elevator e, Ord e, Fractional e) =>
  Pixel RGBA e ->
  Pixel RGBA e ->
  Pixel RGBA e
overlayAlpha bPxa oPxa =
  addAlpha rAlp (liftPx2 f (dropAlpha bPxa) (dropAlpha oPxa))
  where
    bAlp = getAlpha bPxa
    oAlp = getAlpha oPxa
    rAlp = oAlp + bAlp * (1 - oAlp)
    f b o =
      if rAlp == 0
        then 0
        else (o * oAlp + b * bAlp * (1 - oAlp)) / rAlp

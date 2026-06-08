module Kantour.Image
  ( type RGBA
  , type KCImage
  , readImage
  , readImageAuto
  , writeImage
  , Pixel(..)
  ) where

import Data.Massiv.Array
import Data.Massiv.Array.IO

type RGBA e = Pixel (Alpha (SRGB NonLinear)) e
type KCImage e = Array S Ix2 (RGBA e)

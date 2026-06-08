# massiv (v1.0.5.0) — array library

Key types and patterns used in this codebase:

## Import conventions

Two styles used in this codebase:

**Qualified `Ma.` style** (most modules):
```haskell
import qualified Data.Massiv.Array   as Ma
import qualified Data.Massiv.Array.IO as Ma
```

**Open import style** (`Superimpose.hs` only — self-contained massiv module):
```haskell
import Data.Massiv.Array
import Data.Massiv.Array.IO
```

## Types

- `Ma.Array r ix e` — core array type. `r` = representation, `ix` = index, `e` = element.
- `Ma.Sz ix` — size (newtype around `ix`). Constructor: `Ma.Sz :: ix -> Sz ix`.
- `Ma.Ix2` — 2D index: pattern synonym `Ma.Ix2 :: Int -> Int -> Ix2` (avoids `(:.)` operator under qualification).
- `Ma.Comp` — computation strategy: `Ma.Seq`, `Ma.ParOn [Int]`, `Ma.ParN Word16`.
- Representations: `Ma.D` (delayed/pull), `Ma.DW` (delayed/windowed), `Ma.S` (storable), `Ma.U` (unboxed), `Ma.P` (prim), `Ma.B` (boxed), `Ma.BN` (boxed/normal-form), `Ma.BL` (boxed/lazy).

## Key functions

- `Ma.readImage :: (ColorModel cs e, MonadIO m) => FilePath -> m (Ma.Image S cs e)` — reads image in color space determined by target type.
- `Ma.readImageAuto :: (Manifest r (Pixel cs e), ColorSpace cs i e, MonadIO m) => FilePath -> m (Ma.Image r cs e)` — reads with color space conversion from stored format.
- `Ma.writeImage :: (Writable (Image r cs e), MonadIO m) => FilePath -> Ma.Image r cs e -> m ()` — writes to file (format inferred from extension).
- `Ma.makeArrayR :: Load r ix e => r -> Comp -> Sz ix -> (ix -> e) -> Array r ix e` — construct array from index function.
- `Ma.extractM :: (MonadThrow m, Index ix, Source r e) => ix -> Sz ix -> Array r ix e -> m (Array D ix e)` — extract sub-array (monadic, returns delayed).
- `Ma.compute :: (Manifest r e, Load r' ix e) => Array r' ix e -> Array r e` — compute delayed array into manifest.
- `Ma.computeAs :: (Manifest r e, Load r' ix e) => r -> Array r' ix e -> Array r e` — compute into specific representation.
- `Ma.size :: Size r => Array r ix e -> Sz ix` — get array dimensions.
- `(!) :: (Source r e, Index ix) => Array r ix e -> ix -> e` — index element (partial). Used as `img Ma.! Ma.Ix2 i j`.

## Project type aliases

Defined in `Kantour.Image` (shared across `kantour-cli`):

```haskell
type RGBA e = Pixel (Alpha (SRGB NonLinear)) e
type KCImage e = Array S Ix2 (RGBA e)
```

`RGBA e` is the pixel type — used in `Superimpose.hs` (generic over `e`).
`KCImage e` is the full image type — used in `Main.hs` (`KCImage Double`) and `Spritesmith.hs` (`KCImage Word8`).

See also:
- `type Ma.Image r cs e = Matrix r (Pixel cs e) = Array r Ix2 (Pixel cs e)` (from `Data.Massiv.Array.IO`).

## Alpha compositing

**No built-in alpha compositing.** No `overlay`, `superimpose`, `composite`, `blend`, or `alphaBlend` function exists in massiv, massiv-io, Color, or diagrams.

Closest operations (not suitable for offset-aware compositing):
- `zipWith`/`izipWith` — element-wise pairing, requires same-sized arrays, no offset.
- `transform2'` — general multi-array transformation, but no built-in blending support.

Custom implementation using `Ma.makeArrayR Ma.D Ma.Seq` + Porter-Duff "over" formula is required. See `Kantour.MapTwol.Superimpose` for reference.

## `Array S Ix2 (Pixel (Alpha (SRGB NonLinear)) e)` — RGBA images

For RGBA images with sRGB color space and alpha channel. Shorthand: `RGBA e` / `KCImage e` from `Kantour.Image`.

- `Writable PNG (Array S Ix2 (Pixel (Alpha (SRGB NonLinear)) Word8))` and `Word16` exist.
- `Readable PNG (Array S Ix2 (Pixel (Alpha (SRGB NonLinear)) Word8))` and `Word16` exist.
- Writing `Double` pixels requires `readImageAuto` + `ColorSpace` constraint conversion + `Writable (Auto PNG)`.
- `B` (boxed) representation avoids `Storable` constraint on elements.

## Storable chain for `Pixel (Alpha (SRGB NonLinear)) e`

- `Storable e => Storable (Color (SRGB l) e)`
- `(Storable (Color cs e), Storable e) => Storable (Color (Alpha cs) e)`
- `Storable (Color cs e) => Storable (Pixel cs e)`

## Indexing conventions

- Use `Ma.Ix2 y x` (row-major: y first, x second). Avoid `(:.)` operator under qualification.
- `Ma.Sz (Ma.Ix2 h w)` for size; `Sz2` pattern synonym exists but conflicts with type alias.
- Pattern match: `let Ma.Sz (Ma.Ix2 imgH imgW) = Ma.size img`.

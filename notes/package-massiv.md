# massiv (v1.0.5.0) — array library

Key types and patterns used in this codebase:

## Types

- `Array r ix e` — core array type. `r` = representation, `ix` = index, `e` = element.
- `Sz ix` — size (newtype around `ix`). Constructor: `Sz :: ix -> Sz ix`.
- `Ix2` — 2D index: `data Ix2 = Int :. Int`; pattern synonym `Ix2 :: Int -> Int -> Ix2`.
- `Comp` — computation strategy: `Seq`, `ParOn [Int]`, `ParN Word16`.
- Representations: `D` (delayed/pull), `DW` (delayed/windowed), `S` (storable), `U` (unboxed), `P` (prim), `B` (boxed), `BN` (boxed/normal-form), `BL` (boxed/lazy).

## Key functions

- `readImage :: (ColorModel cs e, MonadIO m) => FilePath -> m (Image S cs e)` — reads image in color space determined by target type.
- `readImageAuto :: (Manifest r (Pixel cs e), ColorSpace cs i e, MonadIO m) => FilePath -> m (Image r cs e)` — reads with color space conversion from stored format.
- `writeImage :: (Writable (Image r cs e), MonadIO m) => FilePath -> Image r cs e -> m ()` — writes to file (format inferred from extension).
- `makeArrayR :: Load r ix e => r -> Comp -> Sz ix -> (ix -> e) -> Array r ix e` — construct array from index function.
- `extractM :: (MonadThrow m, Index ix, Source r e) => ix -> Sz ix -> Array r ix e -> m (Array D ix e)` — extract sub-array (monadic, returns delayed).
- `compute :: (Manifest r e, Load r' ix e) => Array r' ix e -> Array r e` — compute delayed array into manifest.
- `computeAs :: (Manifest r e, Load r' ix e) => r -> Array r' ix e -> Array r e` — compute into specific representation.
- `size :: Size r => Array r ix e -> Sz ix` — get array dimensions.
- `(!) :: (Source r e, Index ix) => Array r ix e -> ix -> e` — index element (partial).

## Aliases

- `type Image r cs e = Matrix r (Pixel cs e) = Array r Ix2 (Pixel cs e)` (from `Data.Massiv.Array.IO`).

## `Image S (Alpha (SRGB NonLinear)) e`

For RGBA images with sRGB color space and alpha channel.

- `Writable PNG (Image S (Alpha (SRGB NonLinear)) Word8)` and `Word16` exist.
- `Readable PNG (Image S (Alpha (SRGB NonLinear)) Word8)` and `Word16` exist.
- Writing `Double` pixels requires `readImageAuto` + `ColorSpace` constraint conversion + `Writable (Auto PNG)`.
- `B` (boxed) representation avoids `Storable` constraint on elements.

## Storable chain for `Pixel (Alpha (SRGB NonLinear)) e`

- `Storable e => Storable (Color (SRGB l) e)`
- `(Storable (Color cs e), Storable e) => Storable (Color (Alpha cs) e)`
- `Storable (Color cs e) => Storable (Pixel cs e)`

## Indexing conventions

- Use `Ix2 y x` or `y :. x` (row-major: y first, x second).
- `Sz (h :. w)` for size; `Sz2` pattern synonym exists but conflicts with type alias `type Sz2 = Sz Ix2`.
- Pattern match `Sz (imgH :. imgW) = size img`.

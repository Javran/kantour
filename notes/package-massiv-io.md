# massiv-io (v1.0.0.1) — image I/O for massiv

Imported as `Ma` together with `Data.Massiv.Array`:

```haskell
import qualified Data.Massiv.Array   as Ma
import qualified Data.Massiv.Array.IO as Ma
```

All types/functions used via `Ma.` prefix.

Key re-exports from `Data.Massiv.Array.IO`:

- `Ma.Image` — type alias: `type Image r cs e = Matrix r (Pixel cs e) = Array r Ix2 (Pixel cs e)`.
- `Ma.readImage`, `Ma.readImageAuto`, `Ma.writeImage` — image I/O functions.
- `Ma.Pixel(..)` — `Pixel` type + bundled patterns (`PixelRGBA`, `PixelSRGB`, `PixelCMYK`, etc.).
- `Ma.Alpha` — alpha channel wrapper (type `Alpha :: * -> *`, data `Alpha cs`).
- `Ma.SRGB` — sRGB color space (type `SRGB :: Linearity -> *`, data `SRGB l`).
- `Ma.Linearity(..)` — `Linear | NonLinear`.
- `Ma.Color` — `Color cs e` (internal color representation).

No `Writable`/`Readable` instances for `Double` pixel types directly; use `Auto` format instances which use `ColorSpace cs i e` for conversion.

Color space for RGBA: `Alpha (SRGB NonLinear)`. Project type aliases in `Kantour.Image`:

```haskell
type RGBA e = Pixel (Alpha (SRGB NonLinear)) e
type KCImage e = Array S Ix2 (RGBA e)
```

Source: re-exports from `Graphics.Pixel` (Color package), `Data.Massiv.Array` (massiv), `Graphics.Color.Space`.

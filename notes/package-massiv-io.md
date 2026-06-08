# massiv-io (v1.0.0.1) — image I/O for massiv

Key re-exports from `Data.Massiv.Array.IO`:

- `Image` — type alias: `type Image r cs e = Matrix r (Pixel cs e) = Array r Ix2 (Pixel cs e)`.
- `readImage`, `readImageAuto`, `writeImage` — image I/O functions.
- `Pixel(..)` — `Pixel` type + bundled patterns (`PixelRGBA`, `PixelSRGB`, `PixelCMYK`, etc.).
- `Alpha` — alpha channel wrapper (type `Alpha :: * -> *`, data `Alpha cs`).
- `SRGB` — sRGB color space (type `SRGB :: Linearity -> *`, data `SRGB l`).
- `Linearity(..)` — `Linear | NonLinear`.
- `Color` — `Color cs e` (internal color representation).

No `Writable`/`Readable` instances for `Double` pixel types directly; use `Auto` format instances which use `ColorSpace cs i e` for conversion.

Color space for RGBA: `Alpha (SRGB NonLinear)`.

Source: re-exports from `Graphics.Pixel` (Color package), `Data.Massiv.Array` (massiv), `Graphics.Color.Space`.

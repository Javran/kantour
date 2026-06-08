# Color (v0.4.1) — color spaces and pixels

Provides `Graphics.Pixel` module and related modules.

## Key modules

- `Graphics.Pixel` — `Pixel` newtype, pattern synonyms (`PixelRGBA`, `PixelCMYK`, etc.), `Alpha` re-export.
- `Graphics.Color.Space` — `SRGB` (type `SRGB :: Linearity -> *`), `Linearity(..)` (`Linear | NonLinear`), `ColorSRGB` pattern, `Color` type.
- `Graphics.Pixel.ColorSpace` — color space classes.

## Note

`SRGB` and `NonLinear` are NOT exported from `Graphics.Pixel` — import from `Graphics.Color.Space` instead.
Or import from `Data.Massiv.Array.IO` which re-exports both.

## `Storable` instances for Color types

```
Storable e => Storable (Color (SRGB l) e)
(Storable (Color cs e), Storable e) => Storable (Color (Alpha cs) e)
Storable (Color cs e) => Storable (Pixel cs e)
```

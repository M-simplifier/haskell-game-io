# PNG and BMP compatibility

The Stillroom picker uses the GameIO RGBA8 decoder for these images. PNG accepts
the standard color type and sample depth combinations, transparency, and Adam7.
BMP accepts 1-, 4-, 8-, 16-, 24-, and 32-bit uncompressed rows, and 16/32-bit
RGB bitfields. Images are limited to 8192 pixels per edge and 32 million pixels.

The fixtures cover odd row widths, all five PNG filters, multiple IDAT chunks,
palette and color-key transparency, 16-bit samples, Adam7 passes with no pixels,
BMP palettes and padding, top-down rows, 555/565 masks and 32-bit alpha. Expected
RGBA bytes are derived from the source samples before encoding. Every decoded
image also has a truncated-input check.

```powershell
python tests/image-compatibility-fixtures.py .work/image-compat/fixtures
ghc -O2 -XGHC2021 -isrc -outputdir .work/image-compat/objects tests/ImageCompatibility.hs -o .work/image-compat/image-check.exe
& ./.work/image-compat/image-check.exe .work/image-compat/fixtures
```

All 44 fixture rasters matched with GHC 9.6.7 on Windows. For comparison, the
`stb_image.h` bundled with h-raylib 5.6.0.0 matched 41. The other three used
the 12-byte BMP core header. The Haskell results match the RGBTRIPLE color table
following that header. The comparison decoder read those palettes with a
different length. The comparison tool runs separately from the library.

[PNG specification](https://www.w3.org/TR/png-3/) ·
[BMP core palette](https://learn.microsoft.com/en-us/windows/win32/api/wingdi/ns-wingdi-bitmapcoreinfo) ·
[BMP bitfields](https://learn.microsoft.com/en-us/windows/win32/api/wingdi/ns-wingdi-bitmapinfoheader)

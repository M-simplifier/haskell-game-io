# JPEG decoding

`GameIO.Image.decodeImage` selects JPEG from the file signature and
returns a top-left, straight-alpha RGBA8 image. `decodeJpeg` is the
explicit entry point. The decoder implements 8-bit sequential and
progressive Huffman scans, restart intervals, grayscale, RGB, YCbCr and
four-component Adobe JPEG color transforms. Frame and MCU geometry are
validated before coefficient allocation. The image size limit is 32
million pixels and 8,192 pixels on either axis.

The parser and scan logic follow [ITU-T T.81](https://www.w3.org/Graphics/JPEG/itu-t81.pdf)
Annexes B, F and G. The inverse transform uses a separable cosine basis.
The raster omits only transform terms whose quantized coefficients are
zero. Chroma uses sample-centred linear interpolation at the visible
edges. JPEG metadata is carried by the input file. The raster uses the
encoded orientation and sample values, as in the former Stillroom decoder.

Build the independent comparison runner from the repository root.

```powershell
ghc --make -XGHC2021 -O2 -isrc -outputdir .work/jpeg tests/JpegCheck.hs -o .work/jpeg/jpeg-check.exe
```

Pillow is used to prepare test inputs and reference rasters. This fixture
procedure accepts an existing Stillroom landscape PNG or another RGB image.

```python
from PIL import Image

image = Image.open("scene.png").convert("RGB")
for name, options in {
    "baseline": {"quality": 88, "subsampling": 2},
    "progressive": {"quality": 88, "subsampling": 2, "progressive": True},
    "restart": {"quality": 89, "subsampling": 1, "restart_marker_blocks": 7},
}.items():
    image.save(f"{name}.jpg", **options)
    reference = Image.open(f"{name}.jpg").convert("RGBA")
    with open(f"{name}.rgba", "wb") as output:
        output.write(reference.tobytes())
```

Run `jpeg-check.exe INPUT_JPEG REFERENCE_RGBA8`. It compares every channel
and exercises malformed dimensions, missing EOI and, where present, the
restart sequence.

On the Windows x64 verification host, the 1,672 × 941 Stillroom night-field
scene decoded in about 70 ms with GHC 9.14.1 and `-O2`. The same scene
supplied baseline and progressive 4:2:0 fixtures, cropped 4:4:4 and
4:2:2 inputs, grayscale, RGB and Adobe CMYK. Both restart modes and
odd-width/odd-height chroma edges passed. The largest absolute channel
difference against Pillow/libjpeg was 3 on the 8-bit scale. A 6,001 × 3,999
progressive camera-size fixture derived from the woodland scene decoded in
1.29 seconds, including RGBA conversion. This comparison allows rounding
differences from IDCT and chroma reconstruction.

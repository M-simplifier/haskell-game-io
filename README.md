# Haskell Game IO

Haskell Game IO gives Windows x64 games a Haskell implementation of input,
rendering, text, images, and audio. It calls Win32, Vulkan, and WASAPI directly
through the Haskell FFI. Shader programs, asset decoding, and audio mixing
are implemented in Haskell.

## Build and run

Use GHC and Cabal on Windows x64. The renderer needs a Vulkan 1.3 loader and
driver with dynamic rendering, a graphics/presentation queue, D32 depth,
and either the KHR or EXT pair of surface and swapchain maintenance1
extensions. The Vulkan SDK's validation tools are useful for development.
The library generates SPIR-V itself.

```powershell
cabal build all
cabal test portable-laws audio-mixer graphics-lifetime
cabal run getting-started
```

The standalone build and checks were exercised with GHC 9.14.1. Stillroom
builds the `core`, `graphics`, and `audio` components and the audio worker
with GHC 9.6.7.

## Use in a game

[GettingStarted.hs](examples/GettingStarted.hs) connects ordered input, a
pure state transition, drawing, and a generated tone in one file. Click to
place the marker, use the arrow keys to move it, press Space to change its
colour and play the tone, and press Escape to close the window. The example
opens the device in the example process with `withAudio`.

The default library exposes the complete API. Consumers can also select
`haskell-game-io:core` for input, text, images, UI, and shader descriptions,
`haskell-game-io:graphics` for Win32 and Vulkan, or
`haskell-game-io:audio` for PCM mixing and WASAPI. Build
`exe:gameio-audio-worker` and pass its path to `openIsolatedAudio` to give
the device and sample clock their own process.

- An ordered input trace preserves a quick press and release between two
  rendered frames. A game selects the tick boundary where it consumes each
  event.
- Haskell expressions distinguish vertex and fragment values and lower to
  SPIR-V. Render passes declare which image versions they read and write.
  The renderer uses GPU completion to decide when it can reuse storage.
- The audio mixer schedules commands in samples. Splitting a buffer at a
  command boundary preserves the concatenated output and mixer state.

Text supports TrueType files and collections and OpenType CFF 1 outlines.
Drawing, measurement, and caret placement share a horizontal glyph run
with Unicode scalar clusters. Complex script shaping is outside this version's
text support. Image decoding covers standard PNG color types and sample
depths including Adam7, baseline and progressive Huffman JPEG, and
uncompressed BMP including 16-bit and 32-bit bitfields. The library writes
lossless PNG captures. Audio reads PCM and float WAV, Ogg Vorbis, and MP3.

See the [design contracts](docs/design-contracts.md),
[audio measurements](docs/audio-validation.md),
[image compatibility checks](docs/image-compatibility.md), and
[JPEG comparisons](docs/jpeg-validation.md).

The library is [MIT-licensed](LICENSE). See the
[codec notices](licenses/audio-codecs.txt) and the license terms of any fonts,
images, and audio that you distribute with your application.

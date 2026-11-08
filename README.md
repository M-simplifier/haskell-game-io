# Haskell Game IO

This library targets native Windows x64 games with Win32, Vulkan, and WASAPI.
Game code supplies input rules, draw descriptions, and audio samples in
Haskell. Windows and the graphics driver execute the native operations.

Build with GHC 9.14.1 and Cabal. The native renderer requires a Windows x64
Vulkan driver with swapchain maintenance1 support.

```powershell
cabal build all --offline
cabal test portable-laws audio-mixer graphics-lifetime --offline
cabal run graphics-smoke --offline
```

The default library exposes the complete API. Consumers can also select
`haskell-game-io:core` for pure input, text, image, and draw descriptions,
`haskell-game-io:graphics` for Win32 and Vulkan, or
`haskell-game-io:audio` for PCM mixing and WASAPI. Audio consumers can build
`exe:gameio-audio-worker` and pass its path to `openIsolatedAudio` so the audio
process owns the output device and its sample clock.

The text path reads TrueType files and collections and OpenType CFF 1
outlines. It uses one horizontal glyph run for drawing and measurement,
with scalar cluster indices and an immutable texture atlas. The image path
accepts standard PNG color types and sample depths, including Adam7, baseline
and progressive Huffman JPEG, and uncompressed BMP including 16-bit and
32-bit bitfields. It also writes lossless PNG captures.
Complex script shaping remains open.

The license covers this library. Applications retain the rights and
distribution conditions of their own fonts, images, and audio.

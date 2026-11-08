# Haskell Game IO

Build a Windows x64 game IO library using direct Haskell FFI to Win32, Vulkan,
and WASAPI. Application, binding, renderer, DSP, input, text, and UI code is
Haskell. Do not add custom C/C++/Rust shims, inline-c, raylib, SDL, GLFW,
miniaudio, or a native graphics wrapper. Standard GHC-generated FFI stubs,
GHC RTS/toolchain dependencies, Windows APIs, and the GPU driver are allowed.

The real consumers are Stillroom, Afterlight, and Red Dune. Preserve their
game/product semantics and existing saves. A demonstration is evidence for
its exercised path, not completion of these migrations.

Keep semantic descriptions and native resource execution distinct. Public
invariants must survive every exported operation, including record update.
Use explicit ownership, exception-safe acquisition and release, and GPU
completion-aware retirement. Preserve event order and distinguish simulation,
presentation, and audio time. Use advanced types only where they remove a
concrete error or make a useful composition law expressible.

Shader logic should be authored as Haskell expressions and lowered to SPIR-V;
do not quietly introduce handwritten GLSL/HLSL or an external shader engine.
Validate generated modules and compare meaningful GPU observations against
independent semantic references. Treat all numeric approximation as explicit.

Verify on Windows. Record cold/incremental build timings and meaningful
consumer workloads; do not infer GPU time from wall-clock presentation time.
Measure audio deadline behavior under graphics and allocation load. Native
memory, ABI widths/alignment, thread affinity, callbacks, cancellation, and
device loss are implementation obligations, not generic caveats.

Keep public code and documents free of private brain/session data. Source
imports retain their licenses and provenance. Build products and temporary
inspection captures belong in ignored task directories.

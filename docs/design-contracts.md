# Values, time, and completion

These are design obligations and acceptance criteria. A stated law is not a
claim that an unimplemented path or every native execution already satisfies it.
The first platform is Windows x64 with Vulkan. Multiple APIs or platforms are
not prerequisites for this library.

## Why these representations

The library is more than a set of native function imports. It gives a game
descriptions that can be inspected before execution: ordered input events,
shader expressions, audio commands, and image uses. A native interpreter owns
the corresponding buffers, threads, handles and completion evidence. This
separation is useful only when the descriptions constrain real execution.

For example, a quick press and release is an ordered trace, not just a final
Boolean. Reducing two consecutive trace fragments must compose to the same
result as reducing their concatenation. Audio has a related law with a different
clock: splitting a sample interval must preserve both its output and the state
carried into the next interval. These laws reveal lost edges, duplicated effects,
resampler phase resets and commands applied on the wrong side of a boundary.
They do not require the two domains to share one clock or one universal effect
language.

Shader expressions are Haskell values with stage and value types. Ordinary
Haskell functions can construct a shader; the compiler emits SPIR-V control
flow and instructions from that value. This makes the author's source and its
layout contracts available to ordinary Haskell tools. It does not make a valid
shader physically correct: independent colour and geometry observations still
have to distinguish an intended result from a shared compiler mistake.

On the GPU, immutable descriptions refer to mutable storage. Naming an image's
successive logical values makes an overwrite a dependency, rather than treating
it as an invisible implementation detail. Types and pure planning catch some
misuses before submission. Runtime owner checks and actual completion evidence
handle facts that only Windows and the driver can report. Neither lexical scope
nor garbage collection can substitute for those facts.

The hot execution paths may use pinned buffers, mutation and a separate audio
process. Their observable contracts remain the same. Allocation, latency,
build dependencies and native failure behavior are part of this design, and
must be measured rather than inferred from the use of pure descriptions.

## Accepted input

An accepted input has a host timestamp and an order within that timestamp.
Physical key edges, held state, OS repeats, focus cancellation, IME composition,
and committed text have distinct meanings. SimTick and AudioFrame are not host
timestamps. Conversion and late-input policy are explicit at the consumer.

For the same accepted events and tick boundaries, splitting delivery into
different batches must preserve the resolved transitions. A press at 3 ms and
release at 4 ms must not disappear when presentation next runs at 16 ms.
Repeated presentation must not re-emit a committed game effect.

Afterlight retains its 60 Hz clock, eight-tick work bound, retained debt,
zero-tick input edges, and frozen-clock reset. Red Dune retains its 20 Hz clock,
0.1-second admitted frame delta, sixteen-tick debt bound, eight-tick work bound,
and explicit pause after focus loss. The IO library must express both policies.

## Audio state

At a fixed rate and the same absolute sample command positions, rendering 480
frames and rendering 127 then 353 frames must yield the same concatenated PCM
and final DSP state, when the arithmetic order is unchanged. Commands exactly
at the split expose lost or duplicated transitions. Numerical optimizations
that change arithmetic order require a separate approximation contract.

The production path may own preallocated mutable storage. The law does not
require allocating immutable sample arrays in the device loop. Changes say
which state persists, releases, restarts, or migrates. Device-supplied PCM,
device position, and acoustic output are different observations. Partition
invariance does not establish deadlines; graphics/GC pressure needs measurement.

## GPU values and resource versions

A pass declares the logical versions it reads and writes and the uses of those
values. A small dependency representation lowers these uses to Vulkan execution,
visibility, and layout transitions. Start with one graphics queue and whole
resources; introduce finer ranges or queues only for demonstrated consumers.

A value remains live until all queued readers finish or the plan is cancelled.
This covers overwrites, descriptor changes, and atlas-cell reuse as well as free.
Replacing the glyph A with another glyph while an older frame still references
its atlas cell is an invalid reuse even if the texture has not been destroyed.

Submission completion and presentation completion are different evidence.
CPU scope exit is neither. Frame slots and swapchain images retain separate
ownership. Capability requirements and retirement mechanisms are explicit.
See [Vulkan synchronization](https://docs.vulkan.org/spec/latest/chapters/synchronization.html#synchronization-dependencies)
and [presentation semaphore reuse](https://docs.vulkan.org/guide/latest/swapchain_semaphore_reuse.html).

## Shader meaning and representation

The shader expression language uses types to distinguish useful stage and
value constraints. It lowers to a bounded, inspectable SPIR-V representation.
Vertex, uniform, and storage layouts are distinct contracts; common layout
descriptions derive the corresponding CPU packing and SPIR-V decorations.

CPU interpretation checks compiler consistency. Independently written geometric
or colour references check intended meaning. GPU readback and images check the
native execution. A shared wrong clip conversion must fail the independent
reference even if both generated interpreters agree. Test layout with asymmetric
values. State the tolerance for each observable and the permitted arithmetic.
The official SPIR-V validator is a development tool, not the shader implementation
or a proof of intended meaning.

Local shader bindings also carry an abstract parameter:

```haskell
Expr local stage value
FragmentShader :: (forall local. Expr local 'FragmentStage Vec4) -> FragmentShader
```

Both interpreters pass the same private bound-expression constructor to a
`letExpr` or `foldCount` body. A public client that inspects expression syntax
therefore observes the same form in each interpretation. This matters: passing
a literal in the CPU interpreter and an internal variable in the compiler let
a valid public callback select different colours. The native regression now
exercises that callback with both interpretations.

Closing a shader requires polymorphism in `local`. A local value cannot be
captured into another closed shader, and nominal roles prevent coercing that
boundary away. `evaluateVertexWith` supplies the actual uniforms and returns
all vertex outputs, so the reference can inspect the same public shader that
the native path executes. These are concrete uses of the type system with
observable failure cases, alongside runtime and numerical checks.

## Retention, text, and build units

Retained meshes, materials, fonts and assets are separate from per-frame placement,
colour and time. Rebuilding unchanged geometry or glyph outlines each frame is
not the default API. Glyph placement, advance and cluster data are shared by
drawing, measurement and cursor geometry; ink bounds are not advances. Cache
rebuilding must preserve layout. CPU glyph storage and GPU atlas versions have
different lifetimes.

Keep native declarations, shader compilation, asset processing and pure game
meaning behind separate dependency boundaries. Unchanged generated outputs are
not rewritten. Measure clean, unchanged and leaf-edit builds, plus shader and
asset edits. Measure generation, upload, CPU submission, GPU execution,
presentation/pacing, and audio delivery separately on real consumers.

Stillroom requires CFF outlines in its bundled NotoSansJP.otf, PNG/JPEG/BMP,
WAV/MP3/Ogg, and its existing settings and journey-save meanings. Afterlight and
Red Dune retain their own checkpoints and failure behavior. Red Dune's durable
native store must also lose its authored C layer while retaining its Win32
ownership, flush, readback and immutable-commit contract.

Successful demonstrations exercise these contracts. Completion of the migration
requires Stillroom, Afterlight and Red Dune themselves to use the new IO library.

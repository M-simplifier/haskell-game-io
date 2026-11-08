# Windows audio

Build the game executable and audio worker with the threaded GHC runtime.
For distribution, place the matching `gameio-audio-worker.exe` beside the
game and pass that path to `withIsolatedAudio`. In a Cabal build tree the
worker path comes from `cabal list-bin exe:gameio-audio-worker`.
`withAudio` uses a bound render thread in the current process.

The worker owns the mixer, source cursors and WASAPI shared-mode endpoint.
Commands and bounded Float32 PCM blocks cross private Windows pipes. A
control message has a version, a length and a sequence number. One command
is outstanding at a time. Acknowledgement means the worker accepted that
command. A transport failure or five-second acknowledgement timeout closes
the session. Shutdown acknowledges after closing the device. Parent exit
closes the pipes and ends the worker. Prepared sounds send PCM once and
share it across voices. A stream accepts a whole block or reports zero
accepted frames. Frame counts on a stream use its source rate.

The mixer converts mono and stereo sources to stereo Float32 using linear
interpolation and an integer phase remainder. Gains and pan operate per voice.
Pan preserves both channels at the centre and mutes one at either extreme.
The mixer saturates the sum once before submission. Scheduled starts and stops
use absolute output frames. `readAudioFrame` gives the first frame that has
yet to be reserved for a packet. A command can arrive after that observation,
in which case scheduling returns `AudioFrameAlreadyReserved`.
`readVoice` tracks the mixed source position. The endpoint keeps its own
queue, so these cursors measure different stages of playback.

`openPCMReader` and `readPCMBlock` let music producers decode bounded blocks.
A file source keeps the encoded data and its index in memory. A compressed
seek replays preceding codec state. Whole-file decoding has a 512 MiB PCM
limit. WAV supports integer PCM 8/16/24/32 and Float32, mono or stereo,
including extensible headers. Ogg supports mono/stereo Vorbis I floor 1 and
chains with a common rate and channel layout. MP3 supports mono/stereo
MPEG-1, MPEG-2 and MPEG-2.5 Layer III with indexed bitrates and
LAME-compatible gapless metadata. Free-format MP3, 8 kHz mixed blocks and
Vorbis floor 0 return structured errors. See [codec notices](../licenses/audio-codecs.txt).

## Reproduce the checks

```powershell
cabal test audio-mixer
cabal build exe:gameio-audio-worker exe:audio-smoke
$worker = cabal list-bin exe:gameio-audio-worker
cabal run exe:audio-smoke -- --worker $worker
```

`tests/AudioNative.hs` exercises the same public audio path and the internal
WASAPI loopback probe. Its default run measures two panned tones and measures
again after both voices stop. `--worker WORKER_EXE --stress 20 1250000` mixes
48 voices and measures consecutive endpoint windows during 100 forced major
collections in the controller. Build it with GHC using `-XGHC2021 -O2
-threaded -rtsopts -isrc/native -lole32 -lavrt -lkernel32`. Run the stress
test with `+RTS -N2 -T` for the controller heap statistics.

`tests/AudioCodecs.hs` accepts a WAV, MP3 or Ogg input and the corresponding
interleaved Float32 little-endian reference. It compares complete output
and verifies that irregular block sizes and seeks yield the same PCM. Its
independent references were made with FFmpeg, using synthetic tones, chirps
and transients encoded with libmp3lame and libvorbis. The test tool uses the
same GHC options as AudioNative. FFmpeg is a fixture and comparison tool.

## Observations on Windows x64

- GHC 9.14.1 opened the shared endpoint at 48 kHz Float32 stereo. The
  requested 20 ms buffer became 1,056 frames with a 10 ms device period.
  The loopback probe measured the requested left and right tones, followed by
  silence after stop. This observes the Windows endpoint mix.
- At about 120 MB live controller heap, a run in the same process observed
  three empty-queue events and two padding budget overruns. A separate
  isolated run captured 400 consecutive 10 ms tone windows across the 100
  collections with zero capture discontinuities, empty-queue observations,
  padding overruns or device-period overruns. Maximum measured service time
  was 0.410 ms in that run.
- Two separately scheduled tones agreed with a rational-time reference.
  Rendering 480 frames and splitting at frame 127 gave exactly equal
  Float32 samples and final cursors. Chunked PCM also matched contiguous
  PCM through resampling, including interpolation across chunk boundaries.
  With 48 looping voices, the warmed mixer allocated 23,600 heap bytes
  for each of the 480-frame and 4,800-frame test packets.
- Compared with independent decoded samples, one PCM16 WAV fixture matched
  exactly. Six MP3 fixtures spanning all three MPEG versions agreed in
  frame count and differed by at most 7.31e-7 per Float32 sample. Two Vorbis
  fixtures agreed in frame count and differed by at most 1.79e-7.
- Endpoint reopen, clean shutdown, parent exit and an unresponsive worker
  were exercised. The pipe EOF test closed its worker in 27 ms after parent
  exit. The unresponsive child was rejected and terminated in 5.094 seconds.

The queue and deadline counters describe observed device padding and service
time. They help compare workloads and buffer choices. Acoustic playback,
hardware changes and longer runs have separate observation boundaries.

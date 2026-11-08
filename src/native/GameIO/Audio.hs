-- | Shared-mode native audio for Windows x64. Build the executable with the
-- threaded RTS. Audio time is measured in stereo output frames.
module GameIO.Audio
  ( Audio
  , AudioConfig
  , defaultAudioConfig
  , makeAudioConfig
  , AudioFormat (..)
  , OutputEncoding (..)
  , AudioFrame (..)
  , AudioError (..)
  , AudioStatus (..)
  , AudioStats (..)
  , withAudio
  , openAudio
  , withIsolatedAudio
  , openIsolatedAudio
  , closeAudio
  , readAudioFormat
  , readAudioFrame
  , readAudioStats
  , audioWorkerPid
  , requestAudioDeviceRefresh
  , runAudioWorker
  , Channels (..)
  , PCM
  , pcmSampleRate
  , pcmChannels
  , pcmFrames
  , generatePCM
  , pcmFromSamples
  , slicePCM
  , loadWav
  , decodeWav
  , loadOggVorbis
  , decodeOggVorbis
  , loadMp3
  , decodeMp3
  , PCMSource
  , PCMReader
  , loadWavSource
  , loadMp3Source
  , loadOggVorbisSource
  , sourceSampleRate
  , sourceChannels
  , sourceFrames
  , openPCMReader
  , readPCMBlock
  , Voice
  , VoiceOptions
  , LoopMode (..)
  , defaultVoiceOptions
  , voiceOptions
  , VoiceStatus (..)
  , VoiceSnapshot (..)
  , playSound
  , playSoundAt
  , PreparedSound
  , prepareSound
  , playPreparedSound
  , playPreparedSoundAt
  , readVoice
  , setVoiceMix
  , setVoiceGain
  , setVoicePan
  , pauseVoice
  , resumeVoice
  , stopVoice
  , stopVoiceAt
  , seekVoice
  , AudioStream
  , openStream
  , streamVoice
  , streamWritableFrames
  , writeStream
  , resetStream
  , finishStream
  ) where

import Control.Exception (bracket)
import GameIO.Audio.Codec.Mp3 (decodeMp3, loadMp3, loadMp3Source)
import GameIO.Audio.Codec.Source (PCMReader, PCMSource, openPCMReader, readPCMBlock, sourceChannels, sourceFrames, sourceSampleRate)
import GameIO.Audio.Codec.Vorbis (decodeOggVorbis, loadOggVorbis, loadOggVorbisSource)
import GameIO.Audio.Isolated (runAudioWorker)
import GameIO.Audio.Isolated qualified as Isolated
import GameIO.Audio.Local qualified as Local
import GameIO.Audio.PCM
import GameIO.Audio.Protocol qualified as Wire
import GameIO.Audio.Types
import GameIO.Audio.Wav (decodeWav, loadWav, loadWavSource)

data Audio = LocalAudio !Local.Audio | IsolatedAudio !Isolated.RemoteAudio

data Voice = LocalVoice !Local.Voice | IsolatedVoice !Isolated.RemoteVoice

data AudioStream = LocalStream !Local.AudioStream | IsolatedStream !Isolated.RemoteVoice

data PreparedSound = LocalSound !Local.Audio !PCM | IsolatedSound !Isolated.RemoteSound

-- | Open the default endpoint on a bound thread in the caller's process.
withAudio :: AudioConfig -> (Audio -> IO a) -> IO a
withAudio config = bracket (openAudio config) closeAudio

openAudio :: AudioConfig -> IO Audio
openAudio config = LocalAudio <$> Local.openAudio config

-- | Open the endpoint and mixer in a separate GHC process. Supply the path
-- to the matching gameio-audio-worker executable. Commands and owned PCM
-- travel through private pipes and are acknowledged in order.
withIsolatedAudio :: FilePath -> AudioConfig -> (Audio -> IO a) -> IO a
withIsolatedAudio executable config = bracket (openIsolatedAudio executable config) closeAudio

openIsolatedAudio :: FilePath -> AudioConfig -> IO Audio
openIsolatedAudio executable config = IsolatedAudio <$> Isolated.openRemoteAudio executable config

closeAudio :: Audio -> IO ()
closeAudio (LocalAudio local) = Local.closeAudio local
closeAudio (IsolatedAudio isolated) = Isolated.closeRemoteAudio isolated

readAudioFormat :: Audio -> IO AudioFormat
readAudioFormat (LocalAudio local) = Local.readAudioFormat local
readAudioFormat (IsolatedAudio isolated) = Isolated.remoteFormat isolated

-- | Earliest frame that could accept a scheduled command at observation time.
-- A packet may reserve that frame before a command reaches the mixer.
readAudioFrame :: Audio -> IO AudioFrame
readAudioFrame (LocalAudio local) = Local.readAudioFrame local
readAudioFrame (IsolatedAudio isolated) = Isolated.remoteFrame isolated

readAudioStats :: Audio -> IO AudioStats
readAudioStats (LocalAudio local) = Local.readAudioStats local
readAudioStats (IsolatedAudio isolated) = Isolated.remoteStats isolated

audioWorkerPid :: Audio -> Maybe Int
audioWorkerPid (LocalAudio _) = Nothing
audioWorkerPid (IsolatedAudio isolated) = Isolated.remotePid isolated

-- | Request reopening the current default device. Mixer voices and their
-- source positions survive a reopen. Check deviceRestarts for completion.
requestAudioDeviceRefresh :: Audio -> IO ()
requestAudioDeviceRefresh (LocalAudio local) = Local.requestAudioDeviceRefresh local
requestAudioDeviceRefresh (IsolatedAudio isolated) = Isolated.remoteRefresh isolated

-- | Play a PCM sound at the next packet boundary. In process isolation this
-- copies the clip to the worker. For a continuous source use a bounded stream.
playSound :: Audio -> PCM -> VoiceOptions -> IO Voice
playSound (LocalAudio local) pcm options = LocalVoice <$> Local.playSound local pcm options
playSound (IsolatedAudio isolated) pcm options = IsolatedVoice <$> Isolated.remotePlay isolated Nothing pcm options

-- | Play at an absolute output frame. A command that reaches an already
-- reserved frame fails with AudioFrameAlreadyReserved.
playSoundAt :: Audio -> AudioFrame -> PCM -> VoiceOptions -> IO Voice
playSoundAt (LocalAudio local) frame pcm options = LocalVoice <$> Local.playSoundAt local frame pcm options
playSoundAt (IsolatedAudio isolated) frame pcm options =
  IsolatedVoice <$> Isolated.remotePlay isolated (Just frame) pcm options

-- | Keep one immutable PCM asset for repeated playback. In isolated mode
-- this sends the samples once. Active voices retain their own reference.
prepareSound :: Audio -> PCM -> IO PreparedSound
prepareSound (LocalAudio local) pcm = pure (LocalSound local pcm)
prepareSound (IsolatedAudio isolated) pcm = IsolatedSound <$> Isolated.remotePrepare isolated pcm

playPreparedSound :: PreparedSound -> VoiceOptions -> IO Voice
playPreparedSound (LocalSound local pcm) options = LocalVoice <$> Local.playSound local pcm options
playPreparedSound (IsolatedSound sound) options =
  IsolatedVoice <$> Isolated.remotePlayPrepared sound Nothing options

playPreparedSoundAt :: PreparedSound -> AudioFrame -> VoiceOptions -> IO Voice
playPreparedSoundAt (LocalSound local pcm) frame options =
  LocalVoice <$> Local.playSoundAt local frame pcm options
playPreparedSoundAt (IsolatedSound sound) frame options =
  IsolatedVoice <$> Isolated.remotePlayPrepared sound (Just frame) options

-- | Observe the source cursor mixed into output packets. The endpoint has its
-- own queue and clock. Query isolated voices while their session is open.
readVoice :: Voice -> IO VoiceSnapshot
readVoice (LocalVoice local) = Local.readVoice local
readVoice (IsolatedVoice isolated) = Isolated.remoteVoiceSnapshot isolated

setVoiceMix :: Voice -> Float -> Float -> IO ()
setVoiceMix voice gain pan =
  control voice (\local -> Local.setVoiceMix local gain pan) (Wire.SetMix gain pan)

setVoiceGain :: Voice -> Float -> IO ()
setVoiceGain voice gain = control voice (\local -> Local.setVoiceGain local gain) (Wire.SetGain gain)

setVoicePan :: Voice -> Float -> IO ()
setVoicePan voice pan = control voice (\local -> Local.setVoicePan local pan) (Wire.SetPan pan)

pauseVoice :: Voice -> IO ()
pauseVoice voice = control voice Local.pauseVoice Wire.Pause

resumeVoice :: Voice -> IO ()
resumeVoice voice = control voice Local.resumeVoice Wire.Resume

-- | Idempotently stop a voice. Samples already queued to the endpoint play out.
stopVoice :: Voice -> IO ()
stopVoice voice = control voice Local.stopVoice Wire.Stop

stopVoiceAt :: Voice -> AudioFrame -> IO ()
stopVoiceAt voice frame = control voice (\local -> Local.stopVoiceAt local frame) (Wire.StopAt frame)

seekVoice :: Voice -> Int -> IO ()
seekVoice voice frame = control voice (\local -> Local.seekVoice local frame) (Wire.Seek frame)

control :: Voice -> (Local.Voice -> IO ()) -> Wire.VoiceCommand -> IO ()
control (LocalVoice local) run _ = run local
control (IsolatedVoice isolated) _ operation = Isolated.remoteVoiceControl isolated operation

-- | Attach a bounded PCM queue as a paused voice. Queue initial data and then
-- call resumeVoice on its streamVoice.
openStream :: Audio -> Int -> Channels -> Int -> IO AudioStream
openStream (LocalAudio local) rate channels capacity =
  LocalStream <$> Local.openStream local rate channels capacity
openStream (IsolatedAudio isolated) rate channels capacity =
  IsolatedStream <$> Isolated.remoteOpenStream isolated rate channels capacity

streamVoice :: AudioStream -> Voice
streamVoice (LocalStream local) = LocalVoice (Local.streamVoice local)
streamVoice (IsolatedStream isolated) = IsolatedVoice isolated

streamWritableFrames :: AudioStream -> IO Int
streamWritableFrames (LocalStream local) = Local.streamWritableFrames local
streamWritableFrames (IsolatedStream isolated) = Isolated.remoteAvailable isolated

-- | Enqueue the entire block, or return zero if it would exceed capacity.
writeStream :: AudioStream -> PCM -> IO Int
writeStream (LocalStream local) pcm = Local.writeStream local pcm
writeStream (IsolatedStream isolated) pcm = Isolated.remoteWrite isolated pcm

resetStream :: AudioStream -> IO ()
resetStream (LocalStream local) = Local.resetStream local
resetStream (IsolatedStream isolated) = Isolated.remoteVoiceControl isolated Wire.Reset

finishStream :: AudioStream -> IO ()
finishStream (LocalStream local) = Local.finishStream local
finishStream (IsolatedStream isolated) = Isolated.remoteVoiceControl isolated Wire.Finish

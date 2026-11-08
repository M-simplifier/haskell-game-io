{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module GameIO.Audio.Types
  ( AudioError (..)
  , Channels (..)
  , channelCount
  , AudioConfig (..)
  , AudioFormat (..)
  , OutputEncoding (..)
  , AudioFrame (..)
  , defaultAudioConfig
  , makeAudioConfig
  , AudioStatus (..)
  , AudioStats (..)
  , emptyAudioStats
  , LoopMode (..)
  , VoiceOptions (..)
  , defaultVoiceOptions
  , voiceOptions
  , checkGain
  , checkPan
  , VoiceStatus (..)
  , VoiceSnapshot (..)
  , validateSampleRate
  ) where

import Control.Exception (Exception)
import Data.Binary (Binary)
import Data.Int (Int32)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

data AudioError
  = InvalidSampleRate !Int
  | InvalidFrameCount !Int
  | InvalidBufferDuration !Int
  | InvalidVoiceLimit !Int
  | InvalidGain !Float
  | InvalidPan !Float
  | InvalidSample !Int
  | IncompletePCMFrame
  | InvalidWav !String
  | UnsupportedWavFormat !Word32 !Word32 !Word32
  | InvalidOgg !String
  | InvalidVorbis !String
  | UnsupportedVorbis !String
  | InvalidMp3 !String
  | UnsupportedMp3 !String
  | AudioDecodeTooLarge !Int
  | StreamFormatMismatch
  | StreamFinished
  | VoiceInactive
  | VoiceLimitReached
  | VoiceNotSeekable
  | AudioAssetUnavailable
  | VoicePositionOutOfRange !Int
  | AudioFrameAlreadyReserved !AudioFrame !AudioFrame
  | AudioNotRunning
  | AudioNeedsThreadedRTS
  | WasapiError !String !Int32
  | AudioWin32Error !String !Word32
  | AudioWorkerFailed !String
  | AudioTransportFailure !String
  | AudioWorkerHandleLimit
  | AudioWorkerPCMTooLarge !Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass Binary

instance Exception AudioError

data Channels = Mono | Stereo
  deriving stock (Eq, Show, Generic)
  deriving anyclass Binary

channelCount :: Channels -> Int
channelCount Mono = 1
channelCount Stereo = 2

-- Stream format offered to WASAPI. Shared mode may adapt it to the endpoint.
data AudioConfig = AudioConfig !Int !Int !Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass Binary

-- | Sample rate in Hz, requested device queue in milliseconds, voice capacity.
makeAudioConfig :: Int -> Int -> Int -> Either AudioError AudioConfig
makeAudioConfig rate bufferMilliseconds maxVoices
  | bufferMilliseconds < 1 || bufferMilliseconds > 1000 =
      Left (InvalidBufferDuration bufferMilliseconds)
  | maxVoices < 1 || maxVoices > 1024 = Left (InvalidVoiceLimit maxVoices)
  | otherwise = do
      validateSampleRate rate
      pure (AudioConfig rate bufferMilliseconds maxVoices)

defaultAudioConfig :: AudioConfig
defaultAudioConfig = AudioConfig 48000 20 64

data OutputEncoding = Float32LE
  deriving stock (Eq, Show, Generic)
  deriving anyclass Binary

-- | Absolute frame in the mixer's output clock, independent of simulation ticks.
newtype AudioFrame = AudioFrame Word64
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass Binary

data AudioFormat = AudioFormat
  { audioSampleRate :: !Int
  , audioChannels :: !Channels
  , audioEncoding :: !OutputEncoding
  , audioDeviceBufferFrames :: !Int
  , audioDevicePeriodNs :: !Word64
  , audioEndpointMixRate :: !Int
  , audioMmcssActive :: !Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass Binary

data AudioStatus
  = AudioRunning
  | AudioRecovering !AudioError
  | AudioClosed
  | AudioFailed !AudioError
  deriving stock (Eq, Show, Generic)
  deriving anyclass Binary

data AudioStats = AudioStats
  { audioStatus :: !AudioStatus
  , submittedPackets :: !Word64
  , submittedFrames :: !Word64
  , emptyDeviceQueueEvents :: !Word64
  , exceededPaddingDeadline :: !Word64
  , exceededDevicePeriod :: !Word64
  , maximumServiceNs :: !Word64
  , maximumWakeGapNs :: !Word64
  , streamStarvedFrames :: !Word64
  , deviceRestarts :: !Word64
  , lastDevicePaddingFrames :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass Binary

emptyAudioStats :: AudioStats
emptyAudioStats = AudioStats AudioRunning 0 0 0 0 0 0 0 0 0 0

data LoopMode = PlayOnce | Loop
  deriving stock (Eq, Show, Generic)
  deriving anyclass Binary

-- Balance is linear. Centre retains both channels, endpoints mute one channel.
data VoiceOptions = VoiceOptions !LoopMode !Float !Float
  deriving stock (Eq, Show, Generic)
  deriving anyclass Binary

defaultVoiceOptions :: VoiceOptions
defaultVoiceOptions = VoiceOptions PlayOnce 1 0

voiceOptions :: LoopMode -> Float -> Float -> Either AudioError VoiceOptions
voiceOptions looping gain pan = do
  checkGain gain
  checkPan pan
  pure (VoiceOptions looping gain pan)

checkGain :: Float -> Either AudioError ()
checkGain gain
  | gain >= 0 && gain <= 1 = Right ()
  | otherwise = Left (InvalidGain gain)

checkPan :: Float -> Either AudioError ()
checkPan pan
  | pan >= -1 && pan <= 1 = Right ()
  | otherwise = Left (InvalidPan pan)

data VoiceStatus = VoicePlaying | VoicePaused | VoiceEnded | VoiceStopped | VoiceClosed
  deriving stock (Eq, Show, Generic)
  deriving anyclass Binary

data VoiceSnapshot = VoiceSnapshot
  { voiceStatus :: !VoiceStatus
  , voiceSourceFrame :: !Int
  , voiceSourceRate :: !Int
  , voiceGain :: !Float
  , voicePan :: !Float
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass Binary

validateSampleRate :: Int -> Either AudioError ()
validateSampleRate rate
  | rate > 0 && rate <= 384000 = Right ()
  | otherwise = Left (InvalidSampleRate rate)

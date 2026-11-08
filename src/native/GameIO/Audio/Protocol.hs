{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module GameIO.Audio.Protocol
  ( Request (..)
  , Answer (..)
  , Command (..)
  , Reply (..)
  , VoiceCommand (..)
  , PCMHeader
  , describePayload
  , receivePCM
  , sendPCM
  , sendMessage
  , receiveMessage
  , remoteHandleLimit
  ) where

import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Data.Binary (Binary, decodeOrFail, encode)
import Data.Binary.Get (getWord16le, getWord32le, runGetOrFail)
import Data.Binary.Put (putWord16le, putWord32le, runPut)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Word (Word16, Word32, Word64)
import GHC.Generics (Generic)
import GameIO.Audio.PCM
import GameIO.Audio.Types
import System.IO (Handle, hPutBuf)

data Request = Request !Word64 ![Int] !Command
  deriving stock Generic
  deriving anyclass Binary

data Answer = Answer !Word64 !(Either AudioError Reply)
  deriving stock Generic
  deriving anyclass Binary

data Command
  = Configure !AudioConfig
  | QueryFormat
  | QueryFrame
  | QueryStats
  | RefreshEndpoint
  | Play !(Maybe AudioFrame) !VoiceOptions !PCMHeader
  | Prepare !PCMHeader
  | PlayPrepared !Int !(Maybe AudioFrame) !VoiceOptions
  | OpenStream !Int !Channels !Int
  | WriteStream !Int !PCMHeader
  | ControlVoice !Int !VoiceCommand
  | Shutdown
  deriving stock Generic
  deriving anyclass Binary

data VoiceCommand
  = QueryVoice
  | SetGain !Float
  | SetPan !Float
  | SetMix !Float !Float
  | Pause
  | Resume
  | Stop
  | StopAt !AudioFrame
  | Seek !Int
  | Available
  | Reset
  | Finish
  deriving stock Generic
  deriving anyclass Binary

data Reply
  = Acknowledged
  | Format !AudioFormat
  | Frame !AudioFrame
  | Statistics !AudioStats
  | NewHandle !Int
  | Snapshot !VoiceSnapshot
  | Count !Int
  deriving stock Generic
  deriving anyclass Binary

data PCMHeader = PCMHeader !Int !Channels !Int
  deriving stock Generic
  deriving anyclass Binary

-- Handles held by the client may outlive a one-shot voice. This limit bounds
-- worker-side snapshots independently of the active mixer voice limit.
remoteHandleLimit :: Int
remoteHandleLimit = 4096

maxPCMBytes :: Int
maxPCMBytes = 256 * 1024 * 1024

describePayload :: PCM -> IO PCMHeader
describePayload pcm = do
  let header = PCMHeader (pcmSampleRate pcm) (pcmChannels pcm) (pcmFrames pcm)
  _ <- payloadBytes header
  pure header

receivePCM :: Handle -> PCMHeader -> IO PCM
receivePCM handle header@(PCMHeader rate channels frames) = do
  _ <- payloadBytes header
  readPCMSamples handle rate channels frames

sendPCM :: Handle -> PCM -> IO ()
sendPCM handle pcm = do
  bytes <- describePayload pcm >>= payloadBytes
  withPCMSamples pcm $ \pointer -> hPutBuf handle pointer bytes

payloadBytes :: PCMHeader -> IO Int
payloadBytes (PCMHeader rate channels frames) = do
  either throwIO pure (validateSampleRate rate)
  let bytesPerFrame = channelCount channels * 4
  when (frames <= 0) (throwIO (InvalidFrameCount frames))
  when (frames > maxPCMBytes `quot` bytesPerFrame) (throwIO (AudioWorkerPCMTooLarge frames))
  pure (frames * bytesPerFrame)

magic :: Word32
magic = 0x414f4947 -- GIOA

version :: Word16
version = 1

maxMessageBytes :: Int
maxMessageBytes = 65536

sendMessage :: Binary a => Handle -> a -> IO ()
sendMessage handle message = do
  let payload = encode message
      bytes = BL.length payload
  when (bytes <= 0 || bytes > fromIntegral maxMessageBytes) $
    protocolError "Control message exceeds protocol limit"
  BL.hPut handle $ runPut $ do
    putWord32le magic
    putWord16le version
    putWord32le (fromIntegral bytes)
  BL.hPut handle payload

-- | EOF is valid only between complete messages. No message can commit
-- silently after a truncated body or a mismatched protocol version.
receiveMessage :: Binary a => Handle -> IO (Maybe a)
receiveMessage handle = do
  header <- BS.hGet handle 10
  if BS.null header then pure Nothing else do
    let parseHeader = (,,) <$> getWord32le <*> getWord16le <*> getWord32le
    (prefix, peerVersion, bytes) <- case runGetOrFail parseHeader (BL.fromStrict header) of
      Left _ -> protocolError "Truncated control header"
      Right (_, _, parsed) -> pure parsed
    unless (prefix == magic && peerVersion == version) $
      protocolError "Incompatible audio worker protocol"
    unless (bytes > 0 && bytes <= fromIntegral maxMessageBytes) $
      protocolError "Control message exceeds protocol limit"
    body <- BS.hGet handle (fromIntegral bytes)
    unless (BS.length body == fromIntegral bytes) (protocolError "Truncated control message")
    case decodeOrFail (BL.fromStrict body) of
      Left _ -> protocolError "Malformed control message"
      Right (rest, _, message)
        | BL.null rest -> pure (Just message)
        | otherwise -> protocolError "Trailing control message bytes"

protocolError :: String -> IO a
protocolError = throwIO . AudioTransportFailure

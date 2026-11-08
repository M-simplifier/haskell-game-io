module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (unless)
import GameIO.Audio
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> smoke withAudio
    ["--worker", executable] -> smoke (withIsolatedAudio executable)
    _ -> fail "Usage: audio-smoke [--worker WORKER_EXE]"

smoke :: (AudioConfig -> (Audio -> IO ()) -> IO ()) -> IO ()
smoke withDevice = do
  sound <- tone 44100 523.25 11025
  music <- tone 44100 659.25 17640
  options <- either throwIO pure (voiceOptions PlayOnce 0.5 (-0.6))
  withDevice defaultAudioConfig $ \audio -> do
    readAudioFormat audio >>= print
    print (audioWorkerPid audio)
    voice <- playSound audio sound options
    stream <- openStream audio 44100 Mono 17640
    let deck = streamVoice stream
    setVoiceMix deck 0.4 0.6
    written <- writeStream stream music
    unless (written == pcmFrames music) (fail "The initial stream block was rejected")
    rejected <- writeStream stream music
    unless (rejected == 0) (fail "The full stream accepted more samples")
    finishStream stream
    resumeVoice deck
    threadDelay 90000
    pauseVoice deck
    threadDelay 80000
    resumeVoice deck
    threadDelay 450000
    print =<< readVoice voice
    print =<< readVoice deck
    requestAudioDeviceRefresh audio
    waitForRestart 100 audio
    loopOptions <- either throwIO pure (voiceOptions Loop 0.2 0)
    looping <- playSound audio sound loopOptions
    threadDelay 100000
    stopVoice looping
    snapshot <- readVoice looping
    unless (voiceStatus snapshot == VoiceStopped) (fail "Loop voice did not stop")
    stats <- readAudioStats audio
    unless (submittedPackets stats > 10 && deviceRestarts stats == 1)
      (fail "The native endpoint did not submit or reopen")
    print stats
    closeAudio audio
    closed <- readAudioStats audio
    unless (audioStatus closed == AudioClosed) (fail "The native endpoint did not close")

tone :: Int -> Float -> Int -> IO PCM
tone rate frequency frames = generatePCM rate Mono frames $ \frame ->
  let edge = min 1 (fromIntegral (min frame (frames - frame - 1)) / 200)
      angle = 2 * pi * frequency * fromIntegral frame / fromIntegral rate
  in 0.08 * edge * sin angle

waitForRestart :: Int -> Audio -> IO ()
waitForRestart remaining audio = do
  stats <- readAudioStats audio
  unless (deviceRestarts stats > 0) $
    if remaining == 0 then fail (show stats)
    else threadDelay 10000 >> waitForRestart (remaining - 1) audio

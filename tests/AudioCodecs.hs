module Main (main) where

import Control.Exception (try)
import Control.Monad (unless)
import Data.Bits ((.|.), shiftL, xor)
import Data.ByteString qualified as BS
import Data.Word (Word32)
import Foreign.Storable (peekElemOff)
import GHC.Float (castWord32ToFloat)
import GameIO.Audio
import GameIO.Audio.PCM (withPCMSamples)
import System.Environment (getArgs)
import System.FilePath (takeExtension)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path, oracle] -> checkRecording path oracle
    _ -> fail "Usage: audio-codec-check RECORDING REFERENCE_F32LE"

checkRecording :: FilePath -> FilePath -> IO ()
checkRecording path oracle = do
  let (load, decode) = case takeExtension path of
        ".ogg" -> (loadOggVorbisSource, decodeOggVorbis)
        ".mp3" -> (loadMp3Source, decodeMp3)
        _ -> (loadWavSource, decodeWav)
  source <- load path
  compressed <- BS.readFile path
  reference <- BS.readFile oracle
  whole <- decode compressed
  let channels = if sourceChannels source == Stereo then 2 else 1
      extent = sourceFrames source
  unless (BS.length reference == extent * channels * 4) (fail "Reference frame count differs")
  unless (pcmFrames whole == extent && pcmChannels whole == sourceChannels source
    && pcmSampleRate whole == sourceSampleRate source) (fail "Whole-file PCM metadata differs")
  reader <- openPCMReader source 0
  largest <- checkBlocks reader whole reference channels 0 0 (cycle [127, 353, 4096, 17])
  seek <- openPCMReader source (extent `quot` 3)
  _ <- checkBlocks seek whole reference channels (extent `quot` 3) 0 (repeat 611)
  unless (largest <= 2e-6) (fail ("Independent decoder error " ++ show largest))
  case takeExtension path of
    ".ogg" -> do
      let badChecksum = BS.take 22 compressed <> BS.singleton (BS.index compressed 22 `xor` 1) <> BS.drop 23 compressed
      damaged <- try (decode badChecksum)
      case damaged of
        Left (InvalidOgg _) -> pure ()
        _ -> fail "Corrupted Ogg page checksum was accepted"
    _ -> pure ()
  print (sourceSampleRate source, sourceChannels source, extent, largest)

checkBlocks :: PCMReader -> PCM -> BS.ByteString -> Int -> Int -> Float -> [Int] -> IO Float
checkBlocks reader whole oracle channels = go
  where
    go !offset !largest (wanted : rest) = do
      chunk <- readPCMBlock reader wanted
      case chunk of
        Nothing -> do
          unless (offset == pcmFrames whole) (fail "Unexpected end of PCM stream")
          pure largest
        Just pcm -> do
          let count = pcmFrames pcm
          unless (count == min wanted (pcmFrames whole - offset)) (fail "Unexpected block length")
          peak <- withPCMSamples pcm $ \block ->
            withPCMSamples whole $ \decoded ->
              let compareSamples !index !accumulated
                    | index >= count * channels = pure accumulated
                    | otherwise = do
                        let absolute = offset * channels + index
                        actual <- peekElemOff block index
                        expected <- peekElemOff decoded absolute
                        unless (actual == expected) (fail "Streaming partition changed decoded PCM")
                        compareSamples (index + 1) (max accumulated (abs (actual - oracleSample oracle absolute)))
              in compareSamples 0 largest
          go (offset + count) peak rest
    go _ _ [] = fail "Expected an unbounded stream of requested block sizes"

oracleSample :: BS.ByteString -> Int -> Float
oracleSample bytes index =
  castWord32ToFloat (octet 0 .|. (octet 1 `shiftL` 8) .|. (octet 2 `shiftL` 16) .|. (octet 3 `shiftL` 24))
  where
    octet offset = fromIntegral (BS.index bytes (4 * index + offset)) :: Word32

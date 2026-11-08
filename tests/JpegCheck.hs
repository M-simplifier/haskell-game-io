module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import GHC.Clock (getMonotonicTimeNSec)
import GameIO.Image
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path, oracle] -> compareImage path oracle
    _ -> fail "Usage: jpeg-check INPUT_JPEG REFERENCE_RGBA8"

compareImage :: FilePath -> FilePath -> IO ()
compareImage path oracle = do
  bytes <- BS.readFile path
  reference <- BS.readFile oracle
  start <- getMonotonicTimeNSec
  result <- evaluate (decodeImage bytes)
  image <- either (fail . show) pure result
  finish <- getMonotonicTimeNSec
  let pixels = imageRGBA image
  unless (BS.length pixels == BS.length reference) (fail "Reference raster has a different byte count")
  let (!peak, !total) = difference pixels reference
      mean = fromIntegral total / fromIntegral (BS.length pixels) :: Double
  print (imageSize image, peak, mean, finish - start)
  unless (peak <= 4 && mean <= 0.55) (fail "JPEG pixels diverge from the independent decoder")
  when (BS.drop (BS.length bytes - 2) bytes == BS.pack [255, 217]) $
    rejectInvalid "truncated EOI" (BS.take (BS.length bytes - 2) bytes)
  let (prefix, restart) = BS.breakSubstring (BS.pack [255, 208]) bytes
  unless (BS.null restart) $
    rejectInvalid "incorrect restart order" (prefix <> BS.pack [255, 209] <> BS.drop 2 restart)
  case decodeJpeg oversizedHeader of
    Left (ImageDimensions 8193 1) -> pure ()
    _ -> fail "JPEG dimension cap was bypassed"

rejectInvalid :: String -> BS.ByteString -> IO ()
rejectInvalid label bytes = case decodeJpeg bytes of
  Left (InvalidImage _) -> pure ()
  _ -> fail ("Corrupted JPEG was accepted (" ++ label ++ ")")

oversizedHeader :: BS.ByteString
oversizedHeader = BS.pack [255, 216, 255, 192, 0, 11, 8, 0, 1, 32, 1, 1, 1, 17, 0, 255, 217]

difference :: BS.ByteString -> BS.ByteString -> (Int, Int)
difference bytes reference = go 0 0 0
  where
    go !index !peak !total
      | index == BS.length bytes = (peak, total)
      | otherwise =
          let distance = abs (fromIntegral (BS.index bytes index) - fromIntegral (BS.index reference index))
          in go (index + 1) (max peak distance) (total + distance)

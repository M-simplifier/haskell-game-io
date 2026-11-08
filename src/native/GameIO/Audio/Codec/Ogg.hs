{-# LANGUAGE OverloadedStrings #-}

module GameIO.Audio.Codec.Ogg (OggPacket (..), readOgg) where

import Control.Monad (unless, when)
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Binary.Get
import Data.Bits (shiftL, shiftR, testBit, xor)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Word (Word8, Word32, Word64)
import GameIO.Audio.Types (AudioError (InvalidOgg))

data OggPacket = OggPacket !BS.ByteString !(Maybe Word64) !Bool

data Page = Page !Word8 !Word64 !Word32 !Word32 !Word32 !BS.ByteString !BS.ByteString

-- RFC 3533 framing. A Vorbis link uses one serial and consecutive pages.
-- A chained link restarts at BOS with its own headers and PCM clock.
readOgg :: BS.ByteString -> Either AudioError [[OggPacket]]
readOgg bytes = case runGetOrFail getLinks (BL.fromStrict bytes) of
  Left (_, _, detail) -> Left (InvalidOgg detail)
  Right (_, _, result) -> Right result
  where
    getLinks = do
      done <- isEmpty
      if done then fail "Missing beginning-of-stream page" else links []
    links complete = do
      first@(Page flags _ serial sequenceNumber _ _ _) <- getPage
      unless (testBit flags 1 && not (testBit flags 0) && sequenceNumber == 0) $
        fail "Expected a beginning-of-stream page"
      packets <- getLink serial 0 0 [] [] first
      done <- isEmpty
      if done then pure (reverse (packets : complete)) else links (packets : complete)

    getLink serial expected !partialBytes pending packets (Page flags granule stream number _ sizes body) = do
      unless (stream == serial && number == expected &&
        testBit flags 0 == (partialBytes /= 0) && (expected == 0 || not (testBit flags 1))) $
        fail "Ogg page sequence, serial or packet continuation changed"
      (pieces, continuedBytes, completed) <- unlace (BS.unpack sizes) 0 partialBytes pending []
      let eos = testBit flags 2
          pagePackets = case completed of
            [] -> packets
            final : earlier ->
              OggPacket final (if granule == maxBound then Nothing else Just granule) eos :
                foldr (\packet rest -> OggPacket packet Nothing False : rest) packets earlier
      if eos
        then if continuedBytes == 0 && not (null completed) && granule /= maxBound
          then pure (reverse pagePackets)
          else fail "End-of-stream page lacks a complete packet or granule"
        else getPage >>= getLink serial (expected + 1) continuedBytes pieces pagePackets
      where
        unlace [] !_ !size pieces completed = pure (pieces, size, completed)
        unlace (lace : rest) !offset !size pieces completed = do
          let count = fromIntegral lace
              chunk = BS.take count (BS.drop offset body)
              newSize = size + count
          when (newSize > 16 * 1024 * 1024) (fail "Ogg packet exceeds decoder limit")
          if lace == 255
            then unlace rest (offset + count) newSize (chunk : pieces) completed
            else let packet = BS.concat (reverse (chunk : pieces))
                 in unlace rest (offset + count) 0 [] (packet : completed)

getPage :: Get Page
getPage = do
  (page@(Page _ _ _ _ checksum _ _), original) <- lookAhead $ do
    before <- getRemainingLazyByteString
    case runGetOrFail pageBody before of
      Left (_, _, detail) -> fail detail
      Right (_, consumed, decoded) -> pure (decoded, BL.toStrict (BL.take consumed before))
  unless (oggCRC original == checksum) (fail "Ogg page CRC mismatch")
  skip (BS.length original)
  pure page
  where
    pageBody = do
      tag <- getByteString 4
      version <- getWord8
      flags <- getWord8
      unless (tag == "OggS" && version == 0 && flags < 8) (fail "Invalid Ogg page header")
      granule <- getWord64le
      serial <- getWord32le
      sequenceNumber <- getWord32le
      crc <- getWord32le
      segments <- getWord8 >>= getByteString . fromIntegral
      body <- getByteString (BS.foldl' (\n size -> n + fromIntegral size) 0 segments)
      pure (Page flags granule serial sequenceNumber crc segments body)

oggCRC :: BS.ByteString -> Word32
oggCRC bytes = go 0 0
  where
    extent = BS.length bytes
    go !index !checksum
      | index >= extent = checksum
      | otherwise =
          let octet = if index >= 22 && index < 26 then 0 else BS.index bytes index
              entry = fromIntegral (checksum `shiftR` 24) `xor` fromIntegral octet
          in go (index + 1) ((checksum `shiftL` 8) `xor` crcTable ! entry)

crcTable :: UArray Int Word32
crcTable = listArray (0, 255) [entry 8 (index `shiftL` 24) | index <- [0 .. 255]]
  where
    entry :: Int -> Word32 -> Word32
    entry 0 result = result
    entry n result =
      let rotated = result `shiftL` 1
      in entry (n - 1) (if testBit result 31 then rotated `xor` 0x04c11db7 else rotated)

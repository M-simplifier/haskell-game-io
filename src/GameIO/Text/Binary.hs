module GameIO.Text.Binary
  ( FontError (..), malformed, ensure, slice
  , u8, i8, u16, i16, u32, i32, unsigned
  ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString qualified as BS
import Data.Int (Int8, Int16, Int32)

data FontError
  = TruncatedFont !Int !Int
  | MalformedFont !String
  | UnsupportedFont !String
  | GlyphTooLarge !Int !Int
  deriving (Eq, Show)

malformed :: String -> Either FontError a
malformed = Left . MalformedFont

ensure :: Bool -> String -> Either FontError ()
ensure True _ = Right ()
ensure False label = malformed label

slice :: BS.ByteString -> Int -> Int -> Either FontError BS.ByteString
slice bytes offset size
  | offset < 0 || size < 0 || offset > BS.length bytes - size = Left (TruncatedFont offset size)
  | otherwise = Right (BS.take size (BS.drop offset bytes))

u8 :: BS.ByteString -> Int -> Either FontError Int
u8 bytes offset
  | offset < 0 || offset >= BS.length bytes = Left (TruncatedFont offset 1)
  | otherwise = Right (fromIntegral (BS.index bytes offset))

i8 :: BS.ByteString -> Int -> Either FontError Int
i8 bytes offset = fromIntegral . (fromIntegral :: Int -> Int8) <$> u8 bytes offset

u16, u32 :: BS.ByteString -> Int -> Either FontError Int
u16 bytes offset = unsigned bytes offset 2
u32 bytes offset = unsigned bytes offset 4

i16, i32 :: BS.ByteString -> Int -> Either FontError Int
i16 bytes offset = fromIntegral . (fromIntegral :: Int -> Int16) <$> u16 bytes offset
i32 bytes offset = fromIntegral . (fromIntegral :: Int -> Int32) <$> u32 bytes offset

unsigned :: BS.ByteString -> Int -> Int -> Either FontError Int
unsigned bytes offset count = BS.foldl' step 0 <$> slice bytes offset count
  where step total byte = shiftL total 8 .|. fromIntegral byte

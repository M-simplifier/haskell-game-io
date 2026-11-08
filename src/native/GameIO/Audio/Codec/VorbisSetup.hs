{-# LANGUAGE RankNTypes #-}

module GameIO.Audio.Codec.VorbisSetup
  ( Packet
  , runPacket
  , readBits
  , readFlag
  , packetError
  , integerLog
  , checkedIndex
  , VorbisSetup (..)
  , Mode (..)
  , Mapping (..)
  , Submap (..)
  , Floor (..)
  , FloorClass (..)
  , Residue (..)
  , Book
  , dimensions
  , decodeScalar
  , decodeVector
  , readSetup
  ) where

import Control.Monad (forM, replicateM, replicateM_, unless, when)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import Data.Array (Array, bounds, inRange, listArray, (!))
import Data.Array.Unboxed qualified as U
import Data.Bits
import Data.ByteString qualified as BS
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Word (Word32, Word64)
import GameIO.Audio.Types

type Packet s = ReaderT BS.ByteString (StateT Int (ExceptT AudioError (ST s)))

runPacket :: BS.ByteString -> (forall s. Packet s a) -> Either AudioError a
runPacket bytes action = runST (runExceptT (evalStateT (runReaderT action bytes) 0))

packetError :: AudioError -> Packet s a
packetError = lift . lift . throwE

invalid :: String -> Packet s a
invalid = packetError . InvalidVorbis

readBits :: Int -> Packet s Int
readBits width = do
  offset <- lift get
  bytes <- ask
  let end = offset + width
  when (width < 0 || width > 32 || end > BS.length bytes * 8) (invalid "Truncated Vorbis packet")
  if width == 0 then pure 0 else do
    let (byte, skipBits) = offset `quotRem` 8
        count = (skipBits + width + 7) `quot` 8
        word = loadWord bytes byte count 0 0
        value = (word `shiftR` skipBits) .&. ((1 `shiftL` width) - 1)
    lift (put end)
    pure (fromIntegral value)
  where
    loadWord :: BS.ByteString -> Int -> Int -> Int -> Word64 -> Word64
    loadWord bytes start count !index !word
      | index >= count = word
      | otherwise = loadWord bytes start count (index + 1)
          (word .|. (fromIntegral (BS.index bytes (start + index)) `shiftL` (8 * index)))

readFlag :: Packet s Bool
readFlag = (/= 0) <$> readBits 1

integerLog :: Int -> Int
integerLog x = finiteBitSize x - countLeadingZeros x

checkedIndex :: String -> Array Int a -> Int -> Packet s a
checkedIndex description values index
  | inRange (bounds values) index = pure (values ! index)
  | otherwise = invalid ("Invalid " ++ description ++ " index")

data VorbisSetup = VorbisSetup !(Array Int Mode)

data Mode = Mode !Bool !Mapping

data Mapping = Mapping ![(Int, Int)] ![Int] ![Submap]

data Submap = Submap !Floor !Residue

data Floor = Floor1 !Int !(U.UArray Int Int) ![FloorClass]

data FloorClass = FloorClass !Int !Int !(Maybe Book) !(Array Int (Maybe Book))

data Residue = Residue !Int !Int !Int !Int !Int !Book !(Array (Int, Int) (Maybe Book))

data Book = Book !Int !Huffman !(Maybe Vectors)

data VectorLayout = Lattice | Direct
data Vectors = Vectors !VectorLayout !Int !Float !Float !Bool !(U.UArray Int Int)

-- Minimum free depth lets insertion skip exhausted prefixes. Entry order
-- matters because Vorbis assigns a codeword before reading the next length.
data Huffman = Vacant | Symbol !Int | Branch !Int !Huffman !Huffman

dimensions :: Book -> Int
dimensions (Book count _ _) = count

readSetup :: Int -> BS.ByteString -> Either AudioError VorbisSetup
readSetup channels bytes = runPacket bytes $ do
  books <- readArray 8 readBook
  transforms <- (+ 1) <$> readBits 6
  replicateM_ transforms (checkZero 16 "time transform")
  floors <- readArray 6 (readFloor books)
  residues <- readArray 6 (readResidue books)
  mappings <- readArray 6 (readMapping channels floors residues)
  modes <- readArray 6 $ do
    long <- readFlag
    checkZero 16 "window"
    checkZero 16 "transform"
    mapping <- readBits 8 >>= checkedIndex "mapping" mappings
    pure (Mode long mapping)
  framed <- readFlag
  unless framed (invalid "Missing setup framing bit")
  pure (VorbisSetup modes)
  where
    readArray width parse = do
      count <- (+ 1) <$> readBits width
      listArray (0, count - 1) <$> replicateM count parse

checkZero :: Int -> String -> Packet s ()
checkZero width label = do
  value <- readBits width
  unless (value == 0) (invalid ("Unsupported Vorbis " ++ label))

readBook :: Packet s Book
readBook = do
  magic <- readBits 24
  unless (magic == 0x564342) (invalid "Invalid codebook marker")
  dimension <- readBits 16
  entries <- readBits 24
  unless (dimension > 0 && dimension <= 256 && entries > 0 && entries <= 1048576) $
    invalid "Codebook dimensions or entry count exceed decoder limits"
  ordered <- readFlag
  tree <- if ordered then readOrdered entries else readUnordered entries
  lookupType <- readBits 4
  vectors <- case lookupType of
    0 -> pure Nothing
    1 -> Just <$> readVectors Lattice (lookupCount entries dimension)
    2 -> Just <$> readVectors Direct (entries * dimension)
    _ -> invalid "Invalid codebook lookup type"
  pure (Book dimension tree vectors)
  where
    readVectors layout count = do
      unless (count > 0 && count <= 4194304) (invalid "Codebook vector table exceeds decoder limit")
      minimumValue <- unpackFloat <$> readBits 32
      delta <- unpackFloat <$> readBits 32
      bits <- (+ 1) <$> readBits 4
      accumulate <- readFlag
      unless (all (\value -> not (isInfinite value || isNaN value)) [minimumValue, delta]) $
        invalid "Nonfinite codebook value"
      values <- U.listArray (0, count - 1) <$> replicateM count (readBits bits)
      pure (Vectors layout count minimumValue delta accumulate values)

    readOrdered entries = do
      depth <- (+ 1) <$> readBits 5
      let go !index !lengthBits !tree
            | index == entries = pure tree
            | lengthBits > 32 = invalid "Ordered codeword exceeds 32 bits"
            | otherwise = do
                count <- readBits (integerLog (entries - index))
                when (count > entries - index) (invalid "Ordered codeword run exceeds entry count")
                inserted <- insertRange tree lengthBits index (index + count)
                go (index + count) (lengthBits + 1) inserted
      go 0 depth Vacant

    insertRange !tree depth !index end
      | index >= end = pure tree
      | otherwise = do
          inserted <- insertSymbol depth index tree
          insertRange inserted depth (index + 1) end

    readUnordered entries = do
      sparse <- readFlag
      let go !index !tree
            | index == entries = pure tree
            | otherwise = do
                present <- if sparse then readFlag else pure True
                if present
                  then do
                    depth <- (+ 1) <$> readBits 5
                    inserted <- insertSymbol depth index tree
                    go (index + 1) inserted
                  else go (index + 1) tree
      go 0 Vacant

    insertSymbol depth index = maybe (invalid "Oversubscribed codebook") pure . place depth index

place :: Int -> Int -> Huffman -> Maybe Huffman
place 0 symbol Vacant = Just (Symbol symbol)
place depth symbol tree
  | depth <= 0 = Nothing
  | otherwise = case tree of
      Vacant -> (\left -> branch left Vacant) <$> place (depth - 1) symbol Vacant
      Symbol _ -> Nothing
      Branch _ left right
        | freeDepth left <= depth - 1 -> (\inserted -> branch inserted right) <$> place (depth - 1) symbol left
        | otherwise -> branch left <$> place (depth - 1) symbol right
  where
    branch left right = Branch (1 + min (freeDepth left) (freeDepth right)) left right

freeDepth :: Huffman -> Int
freeDepth Vacant = 0
freeDepth (Symbol _) = 33
freeDepth (Branch depth _ _) = depth

lookupCount :: Int -> Int -> Int
lookupCount entries dimension = search 1 (entries + 1)
  where
    search low high
      | high - low == 1 = low
      | toInteger midpoint ^ dimension <= toInteger entries = search midpoint high
      | otherwise = search low midpoint
      where midpoint = (low + high) `quot` 2

unpackFloat :: Int -> Float
unpackFloat raw = realToFrac (encodeFloat mantissa (power - 788) :: Double)
  where
    word = fromIntegral raw :: Word32
    magnitude = toInteger (word .&. 0x1fffff)
    mantissa = if testBit word 31 then negate magnitude else magnitude
    power = fromIntegral ((word .&. 0x7fe00000) `shiftR` 21)

decodeScalar :: Book -> Packet s Int
decodeScalar (Book _ tree _) = walk tree
  where
    walk Vacant = invalid "Invalid codeword"
    walk (Symbol value) = pure value
    walk (Branch _ left right) = do
      rightBit <- readFlag
      walk (if rightBit then right else left)

decodeVector :: Book -> Packet s [Float]
decodeVector book@(Book dimension _ vectors) = case vectors of
  Nothing -> invalid "A scalar-only codebook was used as a vector"
  Just (Vectors layout count base delta accumulate values) -> do
    symbol <- decodeScalar book
    let linear = dimension * symbol
        valueAt index = fromIntegral (values U.! index) * delta + base
        go !lastValue remaining = case remaining of
          [] -> []
          index : rest ->
            let value = valueAt index + lastValue
            in value : go (if accumulate then value else 0) rest
        lattice !entry = let (next, digit) = entry `quotRem` count in digit : lattice next
        indices = case layout of
          Direct -> [linear .. linear + dimension - 1]
          Lattice -> take dimension (lattice symbol)
    pure (go 0 indices)

readFloor :: Array Int Book -> Packet s Floor
readFloor books = do
  kind <- readBits 16
  unless (kind == 1) (packetError (UnsupportedVorbis ("floor type " ++ show kind)))
  partitions <- readBits 5 >>= flip replicateM (readBits 4)
  let classCount = 1 + List.foldl' max (-1) partitions
  classes <- listArray (0, classCount - 1) <$> replicateM classCount readClass
  plan <- traverse (checkedIndex "floor class" classes) partitions
  multiplier <- (+ 1) <$> readBits 2
  rangeBits <- readBits 4
  let count = 2 + sum [dimension | FloorClass dimension _ _ _ <- plan]
  when (count > 65) (invalid "Too many floor control points")
  xs <- ([0, 1 `shiftL` rangeBits] ++) <$> replicateM (count - 2) (readBits rangeBits)
  when (IntSet.size (IntSet.fromList xs) /= count) (invalid "Duplicate floor control position")
  pure (Floor1 multiplier (U.listArray (0, count - 1) xs) plan)
  where
    readClass = do
      count <- (+ 1) <$> readBits 3
      subclassBits <- readBits 2
      master <- if subclassBits == 0 then pure Nothing else Just <$> readBookIndex
      let choices = 1 `shiftL` subclassBits
      sub <- forM [1 .. choices] $ \_ -> do
        index <- subtract 1 <$> readBits 8
        if index < 0 then pure Nothing else Just <$> checkedIndex "floor codebook" books index
      pure (FloorClass count subclassBits master (listArray (0, choices - 1) sub))
    readBookIndex = readBits 8 >>= checkedIndex "floor master codebook" books

readResidue :: Array Int Book -> Packet s Residue
readResidue books = do
  kind <- readBits 16
  unless (kind <= 2) (invalid "Invalid residue type")
  begin <- readBits 24
  end <- readBits 24
  unless (end >= begin) (invalid "Reversed residue extent")
  partition <- (+ 1) <$> readBits 24
  classes <- (+ 1) <$> readBits 6
  classbook <- readBits 8 >>= checkedIndex "residue classbook" books
  masks <- replicateM classes $ do
    low <- readBits 3
    extend <- readFlag
    high <- if extend then readBits 5 else pure 0
    pure (low .|. (high `shiftL` 3))
  vectorBooks <- concat <$> forM masks (\mask -> forM [0 .. 7] $ \pass ->
    if testBit mask pass
      then Just <$> (readBits 8 >>= checkedIndex "residue vector book" books)
      else pure Nothing)
  pure (Residue kind begin end partition classes classbook
    (listArray ((0, 0), (classes - 1, 7)) vectorBooks))

readMapping :: Int -> Array Int Floor -> Array Int Residue -> Packet s Mapping
readMapping channels floors residues = do
  checkZero 16 "mapping"
  hasSubmaps <- readFlag
  count <- if hasSubmaps then (+ 1) <$> readBits 4 else pure 1
  coupled <- readFlag
  steps <- if coupled then (+ 1) <$> readBits 8 else pure 0
  pairs <- replicateM steps $ do
    magnitude <- readBits (integerLog (channels - 1))
    angle <- readBits (integerLog (channels - 1))
    unless (magnitude < channels && angle < channels && magnitude /= angle) $
      invalid "Invalid Vorbis channel coupling"
    pure (magnitude, angle)
  checkZero 2 "mapping reserved bits"
  mux <- if count > 1 then replicateM channels (readBits 4) else pure (replicate channels 0)
  unless (all (< count) mux) (invalid "Invalid channel submap")
  submaps <- replicateM count $ do
    _ <- readBits 8 -- Unused time-domain selector.
    Submap <$> (readBits 8 >>= checkedIndex "floor" floors)
      <*> (readBits 8 >>= checkedIndex "residue" residues)
  pure (Mapping pairs mux submaps)

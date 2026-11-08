module GameIO.Text.CFF (Cff, parseCff, cffOutline) where

import Control.Monad (foldM)
import Data.Array (Array, bounds, inRange, listArray, (!))
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.IntMap.Strict qualified as IntMap
import GameIO.Text.Binary
import GameIO.Text.Outline

type Index = Array Int BS.ByteString
type Dict = IntMap.IntMap [Maybe Int]

data Locals = OnePrivate !Index | SelectedPrivate !(Array Int Index) !(IntMap.IntMap Int)
data Cff = Cff !Index !Index !Locals

indexLength :: Array Int a -> Int
indexLength = (+ 1) . snd . bounds

indexAt :: String -> Array Int a -> Int -> Either FontError a
indexAt name entries index
  | inRange (bounds entries) index = Right (entries ! index)
  | otherwise = malformed ("CFF " ++ name ++ " index")

emptyIndex :: Index
emptyIndex = listArray (0, -1) []

parseCff :: Int -> BS.ByteString -> Either FontError Cff
parseCff glyphs bytes = do
  major <- u8 bytes 0
  if major /= 1 then Left (UnsupportedFont "Only CFF 1 outlines are supported") else pure ()
  header <- u8 bytes 2
  ensure (header >= 4) "CFF header size"
  (_, topOffset) <- readIndex bytes header
  (tops, stringsOffset) <- readIndex bytes topOffset
  ensure (indexLength tops == 1) "An OpenType CFF must have one Top DICT"
  top <- readDict (tops ! 0)
  (_, globalOffset) <- readIndex bytes stringsOffset
  (global, _) <- readIndex bytes globalOffset
  charStrings <- oneOperand "CharStrings" 17 top
  (chars, _) <- readIndex bytes charStrings
  ensure (glyphs == indexLength chars) "CFF glyph count"
  locals <- case (IntMap.lookup 1236 top, IntMap.lookup 1237 top) of
    (Nothing, Nothing) -> OnePrivate <$> privateSubrs bytes top
    (Just [Just arrayOffset], Just [Just selectOffset]) -> do
      (fontDicts, _) <- readIndex bytes arrayOffset
      let dictCount = indexLength fontDicts
      ensure (dictCount > 0) "CFF FDArray is empty"
      dictionaries <- traverse (readDict . (fontDicts !)) [0 .. dictCount - 1]
      subs <- traverse (privateSubrs bytes) dictionaries
      selector <- readFDSelect bytes selectOffset glyphs dictCount
      pure (SelectedPrivate (listArray (0, dictCount - 1) subs) selector)
    _ -> malformed "CFF requires both FDArray and FDSelect"
  pure (Cff chars global locals)

readIndex :: BS.ByteString -> Int -> Either FontError (Index, Int)
readIndex bytes start = do
  count <- u16 bytes start
  if count == 0 then pure (emptyIndex, start + 2) else do
    width <- u8 bytes (start + 2)
    ensure (width >= 1 && width <= 4) "CFF INDEX offset width"
    let table = start + 3
        body = table + (count + 1) * width
    offsets <- traverse (\i -> unsigned bytes (table + i * width) width) [0 .. count]
    case offsets of
      1 : rest -> do
        end <- foldM ordered 1 rest
        _ <- slice bytes body (end - 1)
        items <- traverse (\(a, b) -> slice bytes (body + a - 1) (b - a)) (zip offsets rest)
        pure (listArray (0, count - 1) items, body + end - 1)
      _ -> malformed "CFF INDEX first offset"
  where
    ordered previous next = next <$ ensure (previous <= next) "CFF INDEX offsets must ascend"

readDict :: BS.ByteString -> Either FontError Dict
readDict bytes = loop 0 [] IntMap.empty
  where
    loop i stack dict
      | i == BS.length bytes = dict <$ ensure (null stack) "CFF DICT has trailing operands"
      | otherwise = do
          code <- u8 bytes i
          case code of
            12 -> do
              extension <- u8 bytes (i + 1)
              loop (i + 2) [] (IntMap.insert (1200 + extension) (reverse stack) dict)
            29 -> do
              number <- i32 bytes (i + 1)
              loop (i + 5) (Just number : stack) dict
            30 -> do
              next <- endReal (i + 1)
              loop next (Nothing : stack) dict
            _ | code >= 28 -> do
                (value, next) <- integer bytes i code
                loop next (Just value : stack) dict
              | otherwise -> loop (i + 1) [] (IntMap.insert code (reverse stack) dict)
    endReal i = do
      value <- u8 bytes i
      if value >= 240 || value .&. 15 == 15 then pure (i + 1) else endReal (i + 1)

integer :: BS.ByteString -> Int -> Int -> Either FontError (Int, Int)
integer bytes i code
  | code == 28 = (, i + 3) <$> i16 bytes (i + 1)
  | code >= 32 && code <= 246 = Right (code - 139, i + 1)
  | code >= 247 && code <= 254 = do
      low <- u8 bytes (i + 1)
      let number = if code <= 250 then (code - 247) * 256 + low + 108 else negate ((code - 251) * 256 + low + 108)
      pure (number, i + 2)
  | otherwise = malformed "Invalid CFF integer"

oneOperand :: String -> Int -> Dict -> Either FontError Int
oneOperand name code dict = case IntMap.lookup code dict of
  Just [Just value] | value >= 0 -> Right value
  _ -> malformed ("CFF " ++ name ++ " operand")

privateSubrs :: BS.ByteString -> Dict -> Either FontError Index
privateSubrs bytes dict = case IntMap.lookup 18 dict of
  Nothing -> Right emptyIndex
  Just [Just size, Just offset] -> do
    private <- slice bytes offset size >>= readDict
    case IntMap.lookup 19 private of
      Nothing -> Right emptyIndex
      Just [Just subOffset] | subOffset >= size -> fst <$> readIndex bytes (offset + subOffset)
      _ -> malformed "CFF local Subrs offset"
  _ -> malformed "CFF Private DICT operands"

readFDSelect :: BS.ByteString -> Int -> Int -> Int -> Either FontError (IntMap.IntMap Int)
readFDSelect bytes offset glyphs dicts = do
  form <- u8 bytes offset
  case form of
    0 -> do
      body <- slice bytes (offset + 1) glyphs
      ensure (BS.all ((< dicts) . fromIntegral) body) "CFF FDSelect index"
      pure (IntMap.fromDistinctAscList (zip [0 ..] (map fromIntegral (BS.unpack body))))
    3 -> do
      count <- u16 bytes (offset + 1)
      ensure (count > 0) "CFF FDSelect ranges"
      let start = offset + 3
      entries <- traverse (\i -> (,) <$> u16 bytes (start + 3*i) <*> u8 bytes (start + 3*i + 2)) [0 .. count - 1]
      end <- u16 bytes (start + 3*count)
      let starts = map fst entries
      ensure (take 1 starts == [0] && end == glyphs && and (zipWith (<) starts (drop 1 starts ++ [end]))) "CFF FDSelect range coverage"
      ensure (all ((< dicts) . snd) entries) "CFF FDSelect index"
      pure (IntMap.fromDistinctAscList entries)
    _ -> Left (UnsupportedFont ("CFF FDSelect format " ++ show form))

data Pen = Pen !Point !(Maybe (Point, [Curve])) !Outline
data Machine = Machine
  { operandStack :: ![Double]
  , hints :: !Int
  , pen :: !Pen
  , budget :: !Int
  }
data Exit = Continue !Machine | Return !Machine | End !Machine

cffOutline :: Cff -> Int -> Either FontError Outline
cffOutline (Cff chars global locals) glyph = do
  main <- indexAt "CharStrings" chars glyph
  local <- case locals of
    OnePrivate entries -> Right entries
    SelectedPrivate entries selector -> case IntMap.lookupLE glyph selector of
      Just (_, index) -> indexAt "FDArray" entries index
      Nothing -> malformed "CFF missing FDSelect"
  result <- execute global local 0 main (Machine [] 0 (Pen (Point 0 0) Nothing []) 100000)
  case result of
    End machine -> pure (outlines (pen machine))
    _ -> malformed "CFF glyph is missing endchar"
  where
    outlines (Pen _ active done) = reverse (finish active done)

execute :: Index -> Index -> Int -> BS.ByteString -> Machine -> Either FontError Exit
execute global local depth bytes = loop 0
  where
    loop i machine
      | budget machine <= 0 = malformed "CFF charstring exceeds operation limit"
      | i == BS.length bytes = Right (Continue machine)
      | otherwise = do
          code <- u8 bytes i
          let next = machine {budget = budget machine - 1}
              args = reverse (operandStack next)
              clean = next {operandStack = []}
              draw transform = transform (pen next) >>= \path -> loop (i + 1) (clean {pen = path})
          case code of
            _ | code == 28 || code >= 32 -> do
                  ensure (length (operandStack machine) < 48) "CFF operand stack overflow"
                  (value, j) <- if code == 255
                    then (, i + 5) . (/ 65536) . fromIntegral <$> i32 bytes (i + 1)
                    else do (n, j) <- integer bytes i code; pure (fromIntegral n, j)
                  loop j (next {operandStack = value : operandStack next})
            1 -> addHints i False next args
            3 -> addHints i False next args
            18 -> addHints i False next args
            23 -> addHints i False next args
            19 -> addHints i True next args
            20 -> addHints i True next args
            4 -> draw (moveAxis (Point 0) args)
            21 -> draw (moveXY args)
            22 -> draw (moveAxis (\a -> Point a 0) args)
            5 -> draw (linePairs args)
            6 -> draw (alternatingLines True args)
            7 -> draw (alternatingLines False args)
            8 -> draw (curveGroups args)
            24 -> draw (curveLine args)
            25 -> draw (lineCurve args)
            26 -> draw (sameAxis False args)
            27 -> draw (sameAxis True args)
            30 -> draw (alternatingCurves False args)
            31 -> draw (alternatingCurves True args)
            10 -> call i local next
            29 -> call i global next
            11 | depth > 0 -> pure (Return next)
            14 | length args <= 1 -> pure (End clean)
               | otherwise -> Left (UnsupportedFont "CFF endchar composite")
            12 -> do
              extension <- u8 bytes (i + 1)
              path <- flex extension args (pen next)
              loop (i + 2) (clean {pen = path})
            _ -> Left (UnsupportedFont ("CFF charstring opcode " ++ show code))
    addHints i withMask machine args = do
      let total = hints machine + div (length args) 2
          skip = if withMask then div (total + 7) 8 else 0
      ensure (total <= 96) "CFF too many stems"
      _ <- slice bytes (i + 1) skip
      loop (i + 1 + skip) (machine {operandStack = [], hints = total})
    call i index machine = case operandStack machine of
      number : remaining -> do
        ensure (depth < 16 && abs number < 1000000000) "CFF subroutine nesting or index"
        let rounded = round number
            count = indexLength index
            bias | count < 1240 = 107
                 | count < 33900 = 1131
                 | otherwise = 32768
        ensure (fromIntegral rounded == number) "CFF fractional subroutine index"
        sub <- indexAt "Subrs" index (rounded + bias)
        returned <- execute global local (depth + 1) sub (machine {operandStack = remaining})
        case returned of
          Return next -> loop (i + 1) next
          End next -> pure (End next)
          Continue _ -> malformed "CFF subroutine is missing return"
      [] -> malformed "CFF missing subroutine index"

finish :: Maybe (Point, [Curve]) -> Outline -> Outline
finish (Just (start, curve' : rest)) done = Contour start (reverse (curve' : rest)) : done
finish _ done = done

move :: Point -> Pen -> Pen
move delta (Pen here active done) =
  let next = addPoint here delta
  in Pen next (Just (next, [])) (finish active done)

moveAxis :: (Double -> Point) -> [Double] -> Pen -> Either FontError Pen
moveAxis delta args path = case widthless 1 args of
  [value] -> Right (move (delta value) path)
  _ -> malformed "CFF moveto operands"

moveXY :: [Double] -> Pen -> Either FontError Pen
moveXY args path = case widthless 2 args of
  [x, y] -> Right (move (Point x y) path)
  _ -> malformed "CFF rmoveto operands"

widthless :: Int -> [a] -> [a]
widthless arity args
  | length args == arity + 1 = drop 1 args
  | otherwise = args

line :: Double -> Double -> Pen -> Either FontError Pen
line x y (Pen here active done) = case active of
  Just (start, previous) ->
    let to = addPoint here (Point x y)
    in Right (Pen to (Just (start, Line to : previous)) done)
  Nothing -> malformed "CFF lineto before moveto"

curve :: Double -> Double -> Double -> Double -> Double -> Double -> Pen -> Either FontError Pen
curve a b c d e f (Pen here active done) = case active of
  Just (start, previous) ->
    let first = addPoint here (Point a b)
        second = addPoint first (Point c d)
        to = addPoint second (Point e f)
    in Right (Pen to (Just (start, Cubic first second to : previous)) done)
  Nothing -> malformed "CFF curveto before moveto"

linePairs, curveGroups, curveLine, lineCurve :: [Double] -> Pen -> Either FontError Pen
linePairs (a:b:rest) path = line a b path >>= if null rest then pure else linePairs rest
linePairs _ _ = malformed "CFF rlineto operands"
curveGroups (a:b:c:d:e:f:rest) path = curve a b c d e f path >>= if null rest then pure else curveGroups rest
curveGroups _ _ = malformed "CFF rrcurveto operands"
curveLine (a:b:c:d:e:f:rest) path = do
  next <- curve a b c d e f path
  case rest of
    [x,y] -> line x y next
    _ -> curveLine rest next
curveLine _ _ = malformed "CFF rcurveline operands"
lineCurve (a:b:rest) path = do
  next <- line a b path
  if length rest == 6 then curveGroups rest next else lineCurve rest next
lineCurve _ _ = malformed "CFF rlinecurve operands"

alternatingLines :: Bool -> [Double] -> Pen -> Either FontError Pen
alternatingLines horizontal (a:rest) path =
  (if horizontal then line a 0 path else line 0 a path) >>=
    if null rest then pure else alternatingLines (not horizontal) rest
alternatingLines _ [] _ = malformed "CFF hlineto/vlineto operands"

sameAxis :: Bool -> [Double] -> Pen -> Either FontError Pen
sameAxis horizontal args path = case args of
  extra : rest | odd (length args) -> go extra rest path
  _ -> go 0 args path
  where
    go extra (a:b:c:d:rest) current = do
      next <- if horizontal then curve a extra b c d 0 current else curve extra a b c 0 d current
      if null rest then pure next else go 0 rest next
    go _ _ _ = malformed "CFF hhcurveto/vvcurveto operands"

alternatingCurves :: Bool -> [Double] -> Pen -> Either FontError Pen
alternatingCurves horizontal (a:b:c:d:rest) path = do
  let extra = case rest of [lastDelta] -> lastDelta; _ -> 0
  next <- if horizontal then curve a 0 b c extra d path else curve 0 a b c d extra path
  if length rest <= 1 then pure next else alternatingCurves (not horizontal) rest next
alternatingCurves _ _ _ = malformed "CFF hvcurveto/vhcurveto operands"

flex :: Int -> [Double] -> Pen -> Either FontError Pen
flex 34 [a,b,c,d,e,f,g] path =
  curve a 0 b c d 0 path >>= curve e 0 f (-c) g 0
flex 35 [a,b,c,d,e,f,g,h,i,j,k,l,_] path =
  curve a b c d e f path >>= curve g h i j k l
flex 36 [a,b,c,d,e,f,g,h,i] path =
  curve a b c d e 0 path >>= curve f 0 g h i (-(b + d + h))
flex 37 [a,b,c,d,e,f,g,h,i,j,k] path =
  let dx = a + c + e + g + i
      dy = b + d + f + h + j
      (x,y) = if abs dx > abs dy then (k, -dy) else (-dx, k)
  in curve a b c d e f path >>= curve g h i j x y
flex opcode _ _ = Left (UnsupportedFont ("CFF escaped charstring opcode or operands " ++ show opcode))

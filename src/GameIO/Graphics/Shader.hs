{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameIO.Graphics.Shader
  ( ShaderStage(..)
  , Expr(VertexPosition, VertexUV, VertexColor, VertexNormal, VertexExtra,
      InstanceRoot, InstanceParameters, InstanceColor, Homogeneous, Transform,
      InterpolatedUV, InterpolatedColor, InterpolatedPosition, InterpolatedNormal,
      InterpolatedSourcePosition, InterpolatedSourceNormal, InterpolatedExtra, Derivative, FrontFacing,
      FragmentCoordinates, SampleTexture, SampleAt, Multiply, Constant4,
      FloatLiteral, MakeVec2, MakeVec3, MakeVec4, MakeMat4, Component,
      FloatUnary, FloatBinary, FloatTernary, ScaleVector, Dot, Length,
      Normalize, Cross, MatrixVector, Compare, BoolLiteral, BoolBinary, BoolNot, Select, If,
      Uniform, UnsignedLiteral, SignedBits, UnsignedFloat, CountFromFloat, UnsignedBinary)
  , FloatShape(..)
  , VectorShape(..)
  , FloatUnaryOp(..)
  , FloatBinaryOp(..)
  , FloatTernaryOp(..)
  , FloatComparison(..)
  , BooleanOp(..)
  , UnsignedOp(..)
  , Component(..)
  , TextureSlot(..)
  , UniformSlot
  , uniformSlot
  , uniformSlots
  , uniformDescriptorSet
  , uniformBytes
  , textureSlotIndex
  , letExpr
  , LoopLimit
  , loopLimit
  , foldCount
  , VertexShader(..)
  , VertexBody(VertexOutputs)
  , letVertex
  , VertexResult(..)
  , DerivativeAxis(..)
  , FragmentSample(..)
  , FragmentQuad(..)
  , FragmentShader(..)
  , defaultVertexShader
  , defaultFragmentShader
  , compileVertexShader
  , compileInstancedVertexShader
  , compileFragmentShader
  , evaluateVertex
  , evaluateVertexWith
  , evaluateVertexSampled
  , evaluateFragment
  , textureBinding
  , textureSet
  ) where

import Control.Monad (forM_)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.Char (ord)
import Data.Word (Word8, Word32)
import GameIO.Graphics.Plan
import GHC.Float (castFloatToWord32)

data ShaderStage = VertexStage | FragmentStage

data FloatShape a where
  Float1 :: FloatShape Float
  Float2 :: FloatShape Vec2
  Float3 :: FloatShape Vec3
  Float4 :: FloatShape Vec4

data VectorShape a where
  Vector2 :: VectorShape Vec2
  Vector3 :: VectorShape Vec3
  Vector4 :: VectorShape Vec4

data Component vector where
  X2 :: Component Vec2
  Y2 :: Component Vec2
  X3 :: Component Vec3
  Y3 :: Component Vec3
  Z3 :: Component Vec3
  X4 :: Component Vec4
  Y4 :: Component Vec4
  Z4 :: Component Vec4
  W4 :: Component Vec4

data FloatUnaryOp = NegateF | Absolute | FloorF | Fract | Sin | Cos | Asin | Exp | Exp2 | Sqrt | InverseSqrt
  deriving (Eq, Show)

data FloatBinaryOp = AddF | SubtractF | MultiplyF | DivideF | Minimum | Maximum | Power | Step | Atan2
  deriving (Eq, Show)

data FloatTernaryOp = Clamp | Mix | Smoothstep deriving (Eq, Show)
data FloatComparison = Less | LessEqual | Greater | EqualF deriving (Eq, Show)
data BooleanOp = Both | EitherTrue deriving (Eq, Show)
data UnsignedOp = MultiplyU | Xor | AndBits | RightShift deriving (Eq, Show)
data TextureSlot = Texture0 | Texture1 | Texture2 | Texture3 deriving (Eq, Ord, Enum, Bounded, Show)
data DerivativeAxis = ScreenX | ScreenY deriving (Eq, Show)

newtype UniformSlot = UniformSlot Word32 deriving (Eq, Ord, Show)
newtype LoopLimit = LoopLimit Word32 deriving (Eq, Ord, Show)

loopLimit :: Word32 -> Maybe LoopLimit
loopLimit bound
  | bound <= 64 = Just (LoopLimit bound)
  | otherwise = Nothing

uniformSlot :: Word32 -> Maybe UniformSlot
uniformSlot slot
  | slot < uniformSlots = Just (UniformSlot slot)
  | otherwise = Nothing

uniformSlots, uniformBytes, uniformDescriptorSet :: Word32
uniformSlots = 48
uniformBytes = uniformSlots * transformColumnStride
uniformDescriptorSet = 4

textureSlotIndex :: TextureSlot -> Word32
textureSlotIndex = fromIntegral . fromEnum

-- Closed shader outputs are polymorphic in local. A body can use a bound
-- value in expressions but cannot evaluate it as a separate closed shader.
type role Expr nominal nominal nominal
data Expr local (stage :: ShaderStage) value where
  VertexPosition :: Expr local 'VertexStage Vec3
  VertexUV :: Expr local 'VertexStage Vec2
  VertexColor :: Expr local 'VertexStage Vec4
  VertexNormal :: Expr local 'VertexStage Vec3
  VertexExtra :: Expr local 'VertexStage Vec2
  InstanceRoot :: Expr local 'VertexStage Vec4
  InstanceParameters :: Expr local 'VertexStage Vec4
  InstanceColor :: Expr local 'VertexStage Vec4
  Homogeneous :: Expr local 'VertexStage Vec3 -> Expr local 'VertexStage Vec4
  Transform :: Expr local 'VertexStage Vec4 -> Expr local 'VertexStage Vec4
  InterpolatedUV :: Expr local 'FragmentStage Vec2
  InterpolatedColor :: Expr local 'FragmentStage Vec4
  InterpolatedPosition :: Expr local 'FragmentStage Vec3
  InterpolatedNormal :: Expr local 'FragmentStage Vec3
  InterpolatedSourcePosition :: Expr local 'FragmentStage Vec3
  InterpolatedSourceNormal :: Expr local 'FragmentStage Vec3
  InterpolatedExtra :: Expr local 'FragmentStage Vec2
  FrontFacing :: Expr local 'FragmentStage Bool
  -- The expression must execute in control flow uniform over the derivative
  -- group, as required for explicit Vulkan fragment derivatives.
  Derivative :: DerivativeAxis -> FloatShape a -> Expr local 'FragmentStage a -> Expr local 'FragmentStage a
  FragmentCoordinates :: Expr local 'FragmentStage Vec4
  SampleTexture :: Expr local 'FragmentStage Vec2 -> Expr local 'FragmentStage Vec4
  SampleAt :: TextureSlot -> Expr local stage Vec2 -> Expr local stage Vec4
  Multiply :: Expr local stage Vec4 -> Expr local stage Vec4 -> Expr local stage Vec4
  Constant4 :: Vec4 -> Expr local stage Vec4
  FloatLiteral :: !Float -> Expr local stage Float
  MakeVec2 :: Expr local stage Float -> Expr local stage Float -> Expr local stage Vec2
  MakeVec3 :: Expr local stage Float -> Expr local stage Float -> Expr local stage Float -> Expr local stage Vec3
  MakeVec4 :: Expr local stage Float -> Expr local stage Float -> Expr local stage Float -> Expr local stage Float
           -> Expr local stage Vec4
  MakeMat4 :: Expr local stage Vec4 -> Expr local stage Vec4 -> Expr local stage Vec4 -> Expr local stage Vec4
           -> Expr local stage Mat4
  Component :: Component a -> Expr local stage a -> Expr local stage Float
  FloatUnary :: FloatShape a -> FloatUnaryOp -> Expr local stage a -> Expr local stage a
  FloatBinary :: FloatShape a -> FloatBinaryOp -> Expr local stage a -> Expr local stage a -> Expr local stage a
  FloatTernary :: FloatShape a -> FloatTernaryOp -> Expr local stage a -> Expr local stage a -> Expr local stage a
               -> Expr local stage a
  ScaleVector :: VectorShape a -> Expr local stage a -> Expr local stage Float -> Expr local stage a
  Dot :: VectorShape a -> Expr local stage a -> Expr local stage a -> Expr local stage Float
  Length :: VectorShape a -> Expr local stage a -> Expr local stage Float
  Normalize :: VectorShape a -> Expr local stage a -> Expr local stage a
  Cross :: Expr local stage Vec3 -> Expr local stage Vec3 -> Expr local stage Vec3
  MatrixVector :: Expr local stage Mat4 -> Expr local stage Vec4 -> Expr local stage Vec4
  Compare :: FloatComparison -> Expr local stage Float -> Expr local stage Float -> Expr local stage Bool
  BoolLiteral :: !Bool -> Expr local stage Bool
  BoolBinary :: BooleanOp -> Expr local stage Bool -> Expr local stage Bool -> Expr local stage Bool
  BoolNot :: Expr local stage Bool -> Expr local stage Bool
  Select :: FloatShape a -> Expr local stage Bool -> Expr local stage a -> Expr local stage a -> Expr local stage a
  If :: FloatShape a -> Expr local stage Bool -> Expr local stage a -> Expr local stage a -> Expr local stage a
  Uniform :: UniformSlot -> Expr local stage Vec4
  UnsignedLiteral :: !Word32 -> Expr local stage Word32
  SignedBits :: Expr local stage Float -> Expr local stage Word32
  UnsignedFloat :: Expr local stage Word32 -> Expr local stage Float
  CountFromFloat :: Expr local stage Float -> Expr local stage Word32
  UnsignedBinary :: UnsignedOp -> Expr local stage Word32 -> Expr local stage Word32 -> Expr local stage Word32
  LetExpr :: Expr local stage a -> (Expr local stage a -> Expr local stage b) -> Expr local stage b
  Bound :: local a -> Expr local stage a
  FoldCount :: FloatShape a -> LoopLimit -> Expr local stage Word32 -> Expr local stage a
            -> (Expr local stage Word32 -> Expr local stage a -> Expr local stage a) -> Expr local stage a

letExpr :: Expr local stage a -> (Expr local stage a -> Expr local stage b) -> Expr local stage b
letExpr = LetExpr

instance Num (Expr local stage Float) where
  (+) = FloatBinary Float1 AddF
  (-) = FloatBinary Float1 SubtractF
  (*) = FloatBinary Float1 MultiplyF
  negate = FloatUnary Float1 NegateF
  abs = FloatUnary Float1 Absolute
  signum x = If Float1 (Compare Less x 0) (-1) (If Float1 (Compare Greater x 0) 1 0)
  fromInteger = FloatLiteral . fromInteger

instance Fractional (Expr local stage Float) where
  (/) = FloatBinary Float1 DivideF
  fromRational = FloatLiteral . fromRational

foldCount :: FloatShape a -> LoopLimit -> Expr local stage Word32 -> Expr local stage a
          -> (Expr local stage Word32 -> Expr local stage a -> Expr local stage a) -> Expr local stage a
foldCount = FoldCount

data VertexShader
  = VertexShader (forall local. Expr local 'VertexStage Vec4)
      (forall local. Expr local 'VertexStage Vec2) (forall local. Expr local 'VertexStage Vec4)
  | WorldVertexShader (forall local. Expr local 'VertexStage Vec4)
      (forall local. Expr local 'VertexStage Vec2) (forall local. Expr local 'VertexStage Vec4)
      (forall local. Expr local 'VertexStage Vec3) (forall local. Expr local 'VertexStage Vec3)
  | SurfaceVertexShader (forall local. VertexBody local)

-- One transformed vertex can feed multiple outputs without replaying its
-- deformation. The same opaque binding rule applies as in scalar expressions.
type role VertexBody nominal
data VertexBody local where
  VertexOutputs :: Expr local 'VertexStage Vec4 -> Expr local 'VertexStage Vec2 -> Expr local 'VertexStage Vec4
                -> Expr local 'VertexStage Vec3 -> Expr local 'VertexStage Vec3 -> Expr local 'VertexStage Vec3
                -> Expr local 'VertexStage Vec3 -> Expr local 'VertexStage Vec2 -> VertexBody local
  VertexLet :: Expr local 'VertexStage a -> (Expr local 'VertexStage a -> VertexBody local) -> VertexBody local

letVertex :: Expr local 'VertexStage a -> (Expr local 'VertexStage a -> VertexBody local) -> VertexBody local
letVertex = VertexLet

vertexBody :: VertexShader -> VertexBody local
vertexBody shader = case shader of
  VertexShader clip uv color -> VertexOutputs clip uv color
    VertexPosition VertexNormal VertexPosition VertexNormal VertexExtra
  WorldVertexShader clip uv color position normal -> VertexOutputs clip uv color
    position normal VertexPosition VertexNormal VertexExtra
  SurfaceVertexShader body -> body

data FragmentShader
  = FragmentShader (forall local. Expr local 'FragmentStage Vec4)
  | MaskedFragmentShader (forall local. Expr local 'FragmentStage Bool) (forall local. Expr local 'FragmentStage Vec4)

data VertexResult = VertexResult
  { resultClipPosition :: !Vec4
  , resultUV :: !Vec2
  , resultColor :: !Color
  , resultWorldPosition :: !Vec3
  , resultWorldNormal :: !Vec3
  , resultSourcePosition :: !Vec3
  , resultSourceNormal :: !Vec3
  , resultExtra :: !Vec2
  } deriving (Eq, Show)

fragmentValue :: FragmentShader -> Expr local 'FragmentStage Vec4
fragmentValue (FragmentShader color) = color
fragmentValue (MaskedFragmentShader _ color) = color

fragmentMask :: FragmentShader -> Maybe (Expr local 'FragmentStage Bool)
fragmentMask (FragmentShader _) = Nothing
fragmentMask (MaskedFragmentShader discard _) = Just discard

defaultVertexShader :: VertexShader
defaultVertexShader = VertexShader (Transform (Homogeneous VertexPosition)) VertexUV VertexColor

defaultFragmentShader :: FragmentShader
defaultFragmentShader = FragmentShader (Multiply InterpolatedColor (SampleTexture InterpolatedUV))

textureBinding, textureSet :: Word32
textureBinding = 0
textureSet = 0

data FragmentSample = FragmentSample
  { sampleUV :: !Vec2
  , sampleColor :: !Color
  , samplePosition :: !Vec3
  , sampleNormal :: !Vec3
  , sampleSourcePosition :: !Vec3
  , sampleSourceNormal :: !Vec3
  , sampleExtra :: !Vec2
  , sampleCoordinates :: !Vec4
  , sampleFrontFacing :: !Bool
  } deriving (Eq, Show)

-- Rows run down from the top of the Vulkan framebuffer.
data FragmentQuad = FragmentQuad !FragmentSample !FragmentSample !FragmentSample !FragmentSample deriving (Eq, Show)

data QuadLane = TopLeft | TopRight | BottomLeft | BottomRight

type TextureLookup = TextureSlot -> Vec2 -> Color
type UniformLookup = UniformSlot -> Vec4

data Inputs stage where
  VertexInputs :: !Mat4 -> !Vertex -> !(Maybe InstanceData) -> TextureLookup -> UniformLookup -> Inputs 'VertexStage
  FragmentInputs :: !FragmentQuad -> !QuadLane -> TextureLookup -> UniformLookup -> Inputs 'FragmentStage

evaluateVertex :: VertexShader -> Mat4 -> Vertex -> (Vec4, Vec2, Color)
evaluateVertex shader matrix vertex =
  let result = evaluateVertexWith shader (const (Vec4 0 0 0 0)) matrix vertex
  in (resultClipPosition result, resultUV result, resultColor result)

evaluateVertexWith :: VertexShader -> (UniformSlot -> Vec4) -> Mat4 -> Vertex -> VertexResult
evaluateVertexWith shader parameters matrix vertex =
  evaluateVertexSampled shader (\_ _ -> Color 1 1 1 1) parameters matrix vertex Nothing

evaluateVertexSampled :: VertexShader -> TextureLookup -> UniformLookup -> Mat4 -> Vertex -> Maybe InstanceData
                      -> VertexResult
evaluateVertexSampled shader textures parameters matrix vertex instanceData = evaluate (vertexBody shader)
  where
    inputs = VertexInputs matrix vertex instanceData textures parameters
    evaluate body = case body of
      VertexLet expression next -> evaluate (next (bindEvaluated inputs (\scope -> interpret scope expression)))
      VertexOutputs clip uv color position normal source sourceN extra -> VertexResult
        (interpret inputs clip) (interpret inputs uv) (vecColor (interpret inputs color))
        (interpret inputs position) (interpret inputs normal) (interpret inputs source)
        (interpret inputs sourceN) (interpret inputs extra)

-- Evaluate the upper-left invocation of a supplied 2x2 fragment quad. Masked
-- fragments return Nothing; all four samples are available to fine derivatives.
evaluateFragment :: FragmentShader -> TextureLookup -> UniformLookup -> FragmentQuad -> Maybe Color
evaluateFragment shader textures parameters quad
  | any (interpret inputs) (fragmentMask shader) = Nothing
  | otherwise = Just (vecColor (interpret inputs (fragmentValue shader)))
  where
    inputs = FragmentInputs quad TopLeft textures parameters

newtype Evaluated stage a = Evaluated (Inputs stage -> a)

bindEvaluated :: Inputs stage -> (Inputs stage -> a) -> Expr (Evaluated stage) stage a
bindEvaluated inputs evaluate = Bound (Evaluated cached)
  where
    cached = case inputs of
      VertexInputs{} -> let result = evaluate inputs in const result
      FragmentInputs quad _ textures uniforms ->
        let at lane = evaluate (FragmentInputs quad lane textures uniforms)
            topLeft = at TopLeft
            topRight = at TopRight
            bottomLeft = at BottomLeft
            bottomRight = at BottomRight
        in \(FragmentInputs _ lane _ _) -> case lane of
          TopLeft -> topLeft
          TopRight -> topRight
          BottomLeft -> bottomLeft
          BottomRight -> bottomRight

interpret :: Inputs stage -> Expr (Evaluated stage) stage a -> a
interpret inputs expression = case expression of
  VertexPosition -> withVertex inputs $ \(position, _, _, _) _ -> position
  VertexUV -> withVertex inputs $ \(_, uv, _, _) _ -> uv
  VertexColor -> withVertex inputs $ \(_, _, color, _) _ -> colorVec color
  VertexNormal -> withVertex inputs $ \(_, _, _, normal) _ -> normal
  VertexExtra -> case inputs of
    VertexInputs _ vertex instanceData _ _ -> maybe (vertexExtra vertex) (const (Vec2 0 0)) instanceData
  InstanceRoot -> withVertex inputs $ \_ -> maybe zero4 (\(InstanceData root _ _ _) -> root)
  InstanceParameters -> withVertex inputs $ \_ -> maybe zero4 (\(InstanceData _ parameters _ _) -> parameters)
  InstanceColor -> withVertex inputs $ \_ -> maybe (Vec4 1 1 1 1) (\(InstanceData _ _ (r,g,b,a) _) ->
    Vec4 (unorm r) (unorm g) (unorm b) (unorm a))
  Homogeneous expr -> case interpret inputs expr of Vec3 x y z -> Vec4 x y z 1
  Transform expr -> case inputs of VertexInputs matrix _ _ _ _ -> transformVec4 matrix (interpret inputs expr)
  InterpolatedUV -> sampleUV (fragmentAt inputs)
  InterpolatedColor -> colorVec (sampleColor (fragmentAt inputs))
  InterpolatedPosition -> samplePosition (fragmentAt inputs)
  InterpolatedNormal -> sampleNormal (fragmentAt inputs)
  InterpolatedSourcePosition -> sampleSourcePosition (fragmentAt inputs)
  InterpolatedSourceNormal -> sampleSourceNormal (fragmentAt inputs)
  InterpolatedExtra -> sampleExtra (fragmentAt inputs)
  FrontFacing -> sampleFrontFacing (fragmentAt inputs)
  FragmentCoordinates -> sampleCoordinates (fragmentAt inputs)
  Derivative axis shape expr -> case inputs of
    FragmentInputs quad lane textures uniforms ->
      let (before, after) = derivativeLanes axis lane
          at next = interpret (FragmentInputs quad next textures uniforms) expr
      in zipFloat shape (-) (at after) (at before)
  SampleTexture expr -> interpret inputs (SampleAt Texture0 expr)
  SampleAt slot expr -> colorVec (texturesFor inputs slot (interpret inputs expr))
  Multiply first second -> multiply4 (interpret inputs first) (interpret inputs second)
  Constant4 literal -> literal
  FloatLiteral literal -> literal
  MakeVec2 x y -> Vec2 (interpret inputs x) (interpret inputs y)
  MakeVec3 x y z -> Vec3 (interpret inputs x) (interpret inputs y) (interpret inputs z)
  MakeVec4 x y z w -> Vec4 (interpret inputs x) (interpret inputs y) (interpret inputs z) (interpret inputs w)
  MakeMat4 x y z w -> mat4FromColumns (interpret inputs x) (interpret inputs y) (interpret inputs z) (interpret inputs w)
  Component member expr -> getComponent member (interpret inputs expr)
  FloatUnary shape op expr -> mapFloat shape (evalUnary op) (interpret inputs expr)
  FloatBinary shape op lhs rhs -> zipFloat shape (evalBinary op) (interpret inputs lhs) (interpret inputs rhs)
  FloatTernary shape op a b c -> zipFloat3 shape (evalTernary op)
    (interpret inputs a) (interpret inputs b) (interpret inputs c)
  ScaleVector shape expr scalar -> mapFloat (vectorFloats shape) (* interpret inputs scalar) (interpret inputs expr)
  Dot shape a b -> dotFloat shape (interpret inputs a) (interpret inputs b)
  Length shape expr -> let argument = interpret inputs expr in sqrt (dotFloat shape argument argument)
  Normalize shape expr ->
    let vector = interpret inputs expr
        magnitude = sqrt (dotFloat shape vector vector)
    in mapFloat (vectorFloats shape) (/ magnitude) vector
  Cross lhs rhs ->
    let Vec3 ax ay az = interpret inputs lhs
        Vec3 bx by bz = interpret inputs rhs
    in Vec3 (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)
  MatrixVector matrix vector -> transformVec4 (interpret inputs matrix) (interpret inputs vector)
  Compare comparison lhs rhs -> evalCompare comparison (interpret inputs lhs) (interpret inputs rhs)
  BoolLiteral literal -> literal
  BoolBinary op lhs rhs -> evalBool op (interpret inputs lhs) (interpret inputs rhs)
  BoolNot expr -> not (interpret inputs expr)
  Select _ condition yes no -> interpret inputs (if interpret inputs condition then yes else no)
  If _ condition yes no -> interpret inputs (if interpret inputs condition then yes else no)
  Uniform slot -> case inputs of
    VertexInputs _ _ _ _ uniformValue -> uniformValue slot
    FragmentInputs _ _ _ uniformValue -> uniformValue slot
  UnsignedLiteral literal -> literal
  SignedBits expr -> fromInteger (truncate (interpret inputs expr))
  UnsignedFloat expr -> fromIntegral (interpret inputs expr)
  CountFromFloat expr ->
    let numeric = interpret inputs expr
    in if isNaN numeric || numeric <= 0 then 0 else
         if numeric >= 64 then 64 else truncate numeric
  UnsignedBinary op lhs rhs -> evalUnsigned op (interpret inputs lhs) (interpret inputs rhs)
  LetExpr valueExpr continuation ->
    interpret inputs (continuation (bindEvaluated inputs (\scope -> interpret scope valueExpr)))
  Bound (Evaluated result) -> result inputs
  FoldCount _ (LoopLimit bound) requested initial body ->
    let count = min bound (interpret inputs requested)
        step index accumulated
          | index >= count = interpret inputs accumulated
          | otherwise = step (index + 1) $ bindEvaluated inputs $ \scope ->
              interpret scope (body (Bound (Evaluated (const index))) accumulated)
    in step 0 (bindEvaluated inputs (\scope -> interpret scope initial))

zero4 :: Vec4
zero4 = Vec4 0 0 0 0

unorm :: Word8 -> Float
unorm byte = fromIntegral byte / 255

withVertex :: Inputs 'VertexStage -> ((Vec3, Vec2, Color, Vec3) -> Maybe InstanceData -> a) -> a
withVertex (VertexInputs _ vertex instanceData _ _) use =
  let raw@(position, _, _, _) = vertexValues vertex
      attrs = case instanceData of
        Nothing -> raw
        Just _ -> (position, Vec2 0 0, Color 1 1 1 1, Vec3 0 1 0)
  in use attrs instanceData

texturesFor :: Inputs stage -> TextureLookup
texturesFor (VertexInputs _ _ _ sample _) = sample
texturesFor (FragmentInputs _ _ sample _) = sample

fragmentAt :: Inputs 'FragmentStage -> FragmentSample
fragmentAt (FragmentInputs (FragmentQuad tl tr bl br) lane _ _) = case lane of
  TopLeft -> tl
  TopRight -> tr
  BottomLeft -> bl
  BottomRight -> br

derivativeLanes :: DerivativeAxis -> QuadLane -> (QuadLane, QuadLane)
derivativeLanes axis lane = case (axis, lane) of
  (ScreenX, TopLeft) -> (TopLeft, TopRight)
  (ScreenX, TopRight) -> (TopLeft, TopRight)
  (ScreenX, _) -> (BottomLeft, BottomRight)
  (ScreenY, TopLeft) -> (TopLeft, BottomLeft)
  (ScreenY, BottomLeft) -> (TopLeft, BottomLeft)
  (ScreenY, _) -> (TopRight, BottomRight)

colorVec :: Color -> Vec4
colorVec (Color r g b a) = Vec4 r g b a

vecColor :: Vec4 -> Color
vecColor (Vec4 r g b a) = Color r g b a

multiply4 :: Vec4 -> Vec4 -> Vec4
multiply4 (Vec4 x y z w) (Vec4 a b c d) = Vec4 (x*a) (y*b) (z*c) (w*d)

vectorFloats :: VectorShape a -> FloatShape a
vectorFloats shape = case shape of Vector2 -> Float2; Vector3 -> Float3; Vector4 -> Float4

mapFloat :: FloatShape a -> (Float -> Float) -> a -> a
mapFloat shape function argument = zipFloat shape (\x _ -> function x) argument argument

zipFloat :: FloatShape a -> (Float -> Float -> Float) -> a -> a -> a
zipFloat shape f lhs rhs = case (shape, lhs, rhs) of
  (Float1, a, b) -> f a b
  (Float2, Vec2 a b, Vec2 x y) -> Vec2 (f a x) (f b y)
  (Float3, Vec3 a b c, Vec3 x y z) -> Vec3 (f a x) (f b y) (f c z)
  (Float4, Vec4 a b c d, Vec4 x y z w) -> Vec4 (f a x) (f b y) (f c z) (f d w)

zipFloat3 :: FloatShape a -> (Float -> Float -> Float -> Float) -> a -> a -> a -> a
zipFloat3 shape f first second third = case (shape, first, second, third) of
  (Float1, a, b, c) -> f a b c
  (Float2, Vec2 a b, Vec2 x y, Vec2 p q) -> Vec2 (f a x p) (f b y q)
  (Float3, Vec3 a b c, Vec3 x y z, Vec3 p q r) -> Vec3 (f a x p) (f b y q) (f c z r)
  (Float4, Vec4 a b c d, Vec4 x y z w, Vec4 p q r s) -> Vec4 (f a x p) (f b y q) (f c z r) (f d w s)

dotFloat :: VectorShape a -> a -> a -> Float
dotFloat shape lhs rhs = case (shape, lhs, rhs) of
  (Vector2, Vec2 a b, Vec2 x y) -> a*x + b*y
  (Vector3, Vec3 a b c, Vec3 x y z) -> a*x + b*y + c*z
  (Vector4, Vec4 a b c d, Vec4 x y z w) -> a*x + b*y + c*z + d*w

getComponent :: Component a -> a -> Float
getComponent member vector = case (member, vector) of
  (X2, Vec2 x _) -> x
  (Y2, Vec2 _ y) -> y
  (X3, Vec3 x _ _) -> x
  (Y3, Vec3 _ y _) -> y
  (Z3, Vec3 _ _ z) -> z
  (X4, Vec4 x _ _ _) -> x
  (Y4, Vec4 _ y _ _) -> y
  (Z4, Vec4 _ _ z _) -> z
  (W4, Vec4 _ _ _ w) -> w

componentIndex :: Component a -> Word32
componentIndex member = case member of
  X2 -> 0
  Y2 -> 1
  X3 -> 0
  Y3 -> 1
  Z3 -> 2
  X4 -> 0
  Y4 -> 1
  Z4 -> 2
  W4 -> 3

evalUnary :: FloatUnaryOp -> Float -> Float
evalUnary op = case op of
  NegateF -> negate
  Absolute -> abs
  FloorF -> floorFloat
  Fract -> \x -> x - floorFloat x
  Sin -> sin
  Cos -> cos
  Asin -> asin
  Exp -> exp
  Exp2 -> (2 **)
  Sqrt -> sqrt
  InverseSqrt -> recip . sqrt
  where
    floorFloat x
      | isNaN x || isInfinite x = x
      | otherwise = fromInteger (floor x)

evalBinary :: FloatBinaryOp -> Float -> Float -> Float
evalBinary op = case op of
  AddF -> (+)
  SubtractF -> (-)
  MultiplyF -> (*)
  DivideF -> (/)
  Minimum -> min
  Maximum -> max
  Power -> (**)
  Step -> \edge x -> if x < edge then 0 else 1
  Atan2 -> atan2

evalTernary :: FloatTernaryOp -> Float -> Float -> Float -> Float
evalTernary op = case op of
  Clamp -> \x lo hi -> max lo (min hi x)
  Mix -> \x y amount -> x * (1 - amount) + y * amount
  Smoothstep -> \lo hi x ->
    let t = max 0 (min 1 ((x - lo) / (hi - lo)))
    in t*t*(3 - 2*t)

evalCompare :: FloatComparison -> Float -> Float -> Bool
evalCompare op = case op of Less -> (<); LessEqual -> (<=); Greater -> (>); EqualF -> (==)

evalBool :: BooleanOp -> Bool -> Bool -> Bool
evalBool op = case op of Both -> (&&); EitherTrue -> (||)

evalUnsigned :: UnsignedOp -> Word32 -> Word32 -> Word32
evalUnsigned op = case op of
  MultiplyU -> (*)
  Xor -> xor
  AndBits -> (.&.)
  RightShift -> \x bits -> x `shiftR` fromIntegral bits

type Id = Word32

newtype Emitted a = Emitted Id

data Instruction = Instruction !Word32 ![Word32]

data Section = ImportDecl | Annotation | Global | FunctionBody

data ModuleState = ModuleState
  { nextId :: !Id
  , extraCapabilities :: ![Word32]
  , extensionImports :: ![Instruction]
  , annotations :: ![Instruction]
  , globals :: ![Instruction]
  , functionBody :: ![Instruction]
  , pointerTypes :: ![((Word32, Id), Id)]
  , globalVariables :: ![Id]
  , samplerVariables :: ![(TextureSlot, Id)]
  , parameterVariable :: !(Maybe Id)
  , activeBlock :: !Id
  }

newtype Emit a = Emit (ModuleState -> (a, ModuleState))

instance Functor Emit where
  fmap f (Emit run) = Emit $ \start ->
    let (a, after) = run start in (f a, after)

instance Applicative Emit where
  pure a = Emit (\state -> (a, state))
  Emit runFunction <*> Emit runValue = Emit $ \start ->
    let (f, middle) = runFunction start
        (argument, after) = runValue middle
    in (f argument, after)

instance Monad Emit where
  Emit run >>= next = Emit $ \start ->
    let (a, middle) = run start
        Emit finish = next a
    in finish middle

newId :: Emit Id
newId = Emit $ \state ->
  let identifier = nextId state
  in (identifier, state { nextId = identifier + 1 })

emit :: Section -> Word32 -> [Word32] -> Emit ()
emit section opcode operands = Emit $ \state ->
  let instruction = Instruction opcode operands
      updated = case section of
        ImportDecl -> state { extensionImports = instruction : extensionImports state }
        Annotation -> state { annotations = instruction : annotations state }
        Global -> state { globals = instruction : globals state }
        FunctionBody -> state { functionBody = instruction : functionBody state }
  in ((), updated)

requireCapability :: Word32 -> Emit ()
requireCapability capability = Emit $ \state ->
  let known = extraCapabilities state
  in ((), if capability `elem` known then state else state { extraCapabilities = capability : known })

define :: Word32 -> [Word32] -> Emit Id
define opcode operands = do
  identifier <- newId
  emit Global opcode (identifier : operands)
  pure identifier

declare :: Word32 -> Id -> [Word32] -> Emit Id
declare opcode resultType operands = do
  identifier <- newId
  emit Global opcode (resultType : identifier : operands)
  pure identifier

value :: Word32 -> Id -> [Word32] -> Emit Id
value opcode resultType operands = do
  identifier <- newId
  emit FunctionBody opcode (resultType : identifier : operands)
  pure identifier

data Types = Types
  { voidType :: !Id
  , floatType :: !Id
  , unsignedType :: !Id
  , signedType :: !Id
  , boolType :: !Id
  , vec2Type :: !Id
  , vec3Type :: !Id
  , vec4Type :: !Id
  , mat4Type :: !Id
  , functionType :: !Id
  , extLibrary :: !Id
  }

declareTypes :: Emit Types
declareTypes = do
  voidTy <- define OpTypeVoid []
  floatTy <- define OpTypeFloat [32]
  unsignedTy <- define OpTypeInt [32, 0]
  signedTy <- define OpTypeInt [32, 1]
  boolTy <- define OpTypeBool []
  vec2Ty <- define OpTypeVector [floatTy, 2]
  vec3Ty <- define OpTypeVector [floatTy, 3]
  vec4Ty <- define OpTypeVector [floatTy, 4]
  mat4Ty <- define OpTypeMatrix [vec4Ty, 4]
  funcTy <- define OpTypeFunction [voidTy]
  glsl <- newId
  emit ImportDecl OpExtInstImport (glsl : encodeString "GLSL.std.450")
  pure (Types voidTy floatTy unsignedTy signedTy boolTy vec2Ty vec3Ty vec4Ty mat4Ty funcTy glsl)

variable :: Word32 -> Id -> Emit Id
variable storage ty = do
  pointerType <- pointerTypeFor storage ty
  identifier <- declare OpVariable pointerType [storage]
  Emit (\state -> (identifier, state { globalVariables = identifier : globalVariables state }))

pointerTypeFor :: Word32 -> Id -> Emit Id
pointerTypeFor storage ty = Emit $ \state ->
  case lookup key (pointerTypes state) of
    Just identifier -> (identifier, state)
    Nothing ->
      let Emit definePointer = define OpTypePointer [storage, ty]
          (identifier, updated) = definePointer state
      in (identifier, updated { pointerTypes = (key, identifier) : pointerTypes updated })
  where
    key = (storage, ty)

decorate :: Id -> Word32 -> [Word32] -> Emit ()
decorate identifier kind extra = emit Annotation OpDecorate (identifier : kind : extra)

location :: Word32 -> Id -> Word32 -> Emit Id
location storage ty number = do
  identifier <- variable storage ty
  decorate identifier decorateLocation [number]
  pure identifier

data VertexStreams
  = OrdinaryStream !Id !Id !Id !Id
  | InstancedStream !Id !Id !Id

data VertexVariables = VertexVariables
  { vertexPositionId :: !Id
  , vertexStreams :: !VertexStreams
  , pushId :: !Id
  , matrixPointerId :: !Id
  , zeroId :: !Id
  , oneId :: !Id
  , vertexSampledType :: !Id
  }

data FragmentVariables = FragmentVariables
  { fragmentUVId :: !Id
  , fragmentColorId :: !Id
  , fragmentPositionId :: !Id
  , fragmentNormalId :: !Id
  , fragmentSourcePositionId :: !Id
  , fragmentSourceNormalId :: !Id
  , fragmentExtraId :: !Id
  , fragmentCoordinateId :: !Id
  , fragmentFrontFacingId :: !Id
  , sampledImageType :: !Id
  }

data Environment stage where
  VertexEnv :: !Types -> !VertexVariables -> Environment 'VertexStage
  FragmentEnv :: !Types -> !FragmentVariables -> Environment 'FragmentStage

compileVertexShader :: VertexShader -> [Word32]
compileVertexShader = compileVertex False

compileInstancedVertexShader :: VertexShader -> [Word32]
compileInstancedVertexShader = compileVertex True

compileVertex :: Bool -> VertexShader -> [Word32]
compileVertex instanced shader = compile stageVertex [] $ \entry -> do
  types <- declareTypes
  sampledTy <- declareSampledImage types
  zero <- declare OpConstant (unsignedType types) [0]
  one <- declare OpConstant (floatType types) [castFloatToWord32 1]
  positionIn <- location storageInput (vec3Type types) positionLocation
  streams <- if instanced
    then InstancedStream
      <$> location storageInput (vec4Type types) instanceRootLocation
      <*> location storageInput (vec4Type types) instanceParametersLocation
      <*> location storageInput (vec4Type types) instanceColorLocation
    else OrdinaryStream
      <$> location storageInput (vec2Type types) uvLocation
      <*> location storageInput (vec4Type types) colorLocation
      <*> location storageInput (vec3Type types) normalLocation
      <*> location storageInput (vec2Type types) vertexExtraLocation
  positionOut <- variable storageOutput (vec4Type types)
  decorate positionOut decorateBuiltIn [builtInPosition]
  uvOut <- location storageOutput (vec2Type types) varyingUVLocation
  colorOut <- location storageOutput (vec4Type types) varyingColorLocation
  worldOut <- location storageOutput (vec3Type types) varyingPositionLocation
  normalOut <- location storageOutput (vec3Type types) varyingNormalLocation
  sourceOut <- location storageOutput (vec3Type types) varyingSourcePositionLocation
  sourceNormalOut <- location storageOutput (vec3Type types) varyingSourceNormalLocation
  extraOut <- location storageOutput (vec2Type types) varyingExtraLocation
  pushType <- define OpTypeStruct [mat4Type types]
  decorate pushType decorateBlock []
  emit Annotation OpMemberDecorate [pushType, 0, decorateColMajor]
  emit Annotation OpMemberDecorate [pushType, 0, decorateMatrixStride, transformColumnStride]
  emit Annotation OpMemberDecorate [pushType, 0, decorateOffset, transformOffset]
  push <- variable storagePushConstant pushType
  matrixPtr <- pointerTypeFor storagePushConstant (mat4Type types)
  let env = VertexEnv types (VertexVariables positionIn streams push matrixPtr zero one sampledTy)
  beginFunction types entry
  let store :: Id -> Expr Emitted 'VertexStage a -> Emit ()
      store destination expression = lower env expression >>= \identifier -> emit FunctionBody OpStore [destination, identifier]
  let renderVertex body = case body of
        VertexLet expression next -> lower env expression >>= \identifier -> renderVertex (next (Bound (Emitted identifier)))
        VertexOutputs clip uv color position normal source sourceN extra -> do
          store positionOut clip
          store uvOut uv
          store colorOut color
          store worldOut position
          store normalOut normal
          store sourceOut source
          store sourceNormalOut sourceN
          store extraOut extra
  renderVertex (vertexBody shader)
  endFunction

compileFragmentShader :: FragmentShader -> [Word32]
compileFragmentShader shader = compile stageFragment [originUpperLeft] $ \entry -> do
  types <- declareTypes
  sampledTy <- declareSampledImage types
  uvIn <- location storageInput (vec2Type types) varyingUVLocation
  colorIn <- location storageInput (vec4Type types) varyingColorLocation
  positionIn <- location storageInput (vec3Type types) varyingPositionLocation
  normalIn <- location storageInput (vec3Type types) varyingNormalLocation
  sourceIn <- location storageInput (vec3Type types) varyingSourcePositionLocation
  sourceNormalIn <- location storageInput (vec3Type types) varyingSourceNormalLocation
  extraIn <- location storageInput (vec2Type types) varyingExtraLocation
  coord <- variable storageInput (vec4Type types)
  decorate coord decorateBuiltIn [builtInFragmentCoordinate]
  facing <- variable storageInput (boolType types)
  decorate facing decorateBuiltIn [builtInFrontFacing]
  colorOut <- location storageOutput (vec4Type types) 0
  let env = FragmentEnv types (FragmentVariables uvIn colorIn positionIn normalIn sourceIn sourceNormalIn extraIn coord facing sampledTy)
  beginFunction types entry
  -- Color computations include explicit derivatives. Finish them while the
  -- derivative group is still intact, then discard masked invocations.
  color <- lower env (fragmentValue shader)
  forM_ (fragmentMask shader) $ \mask -> do
    discard <- lower env mask
    hidden <- newId
    visible <- newId
    emit FunctionBody OpSelectionMerge [visible, 0]
    emit FunctionBody OpBranchConditional [discard, hidden, visible]
    labelBlock hidden
    emit FunctionBody OpKill []
    labelBlock visible
  emit FunctionBody OpStore [colorOut, color]
  endFunction

declareSampledImage :: Types -> Emit Id
declareSampledImage types = do
  imageTy <- define OpTypeImage [floatType types, image2D, 0, 0, 0, 1, unknownImageFormat]
  define OpTypeSampledImage [imageTy]

beginFunction :: Types -> Id -> Emit ()
beginFunction types entry = do
  emit FunctionBody OpFunction [voidType types, entry, 0, functionType types]
  label <- newId
  labelBlock label

labelBlock :: Id -> Emit ()
labelBlock identifier = do
  emit FunctionBody OpLabel [identifier]
  Emit (\state -> ((), state { activeBlock = identifier }))

currentBlock :: Emit Id
currentBlock = Emit (\state -> (activeBlock state, state))

endFunction :: Emit ()
endFunction = emit FunctionBody OpReturn [] >> emit FunctionBody OpFunctionEnd []

lower :: forall stage a. Environment stage -> Expr Emitted stage a -> Emit Id
lower env expr = case expr of
  VertexPosition -> case env of
    VertexEnv _ vars -> load (vec3Type types) (vertexPositionId vars)
  VertexUV -> case env of
    VertexEnv _ vars -> case vertexStreams vars of
      OrdinaryStream uv _ _ _ -> load (vec2Type types) uv
      InstancedStream{} -> lower env (MakeVec2 0 0)
  VertexColor -> case env of
    VertexEnv _ vars -> case vertexStreams vars of
      OrdinaryStream _ color _ _ -> load (vec4Type types) color
      InstancedStream{} -> lower env (Constant4 (Vec4 1 1 1 1))
  VertexNormal -> case env of
    VertexEnv _ vars -> case vertexStreams vars of
      OrdinaryStream _ _ normal _ -> load (vec3Type types) normal
      InstancedStream{} -> lower env (MakeVec3 0 1 0)
  VertexExtra -> case env of
    VertexEnv _ vars -> case vertexStreams vars of
      OrdinaryStream _ _ _ extra -> load (vec2Type types) extra
      InstancedStream{} -> lower env (MakeVec2 0 0)
  InstanceRoot -> case env of
    VertexEnv _ vars -> case vertexStreams vars of
      OrdinaryStream{} -> lower env (Constant4 zero4)
      InstancedStream root _ _ -> load (vec4Type types) root
  InstanceParameters -> case env of
    VertexEnv _ vars -> case vertexStreams vars of
      OrdinaryStream{} -> lower env (Constant4 zero4)
      InstancedStream _ parameters _ -> load (vec4Type types) parameters
  InstanceColor -> case env of
    VertexEnv _ vars -> case vertexStreams vars of
      OrdinaryStream{} -> lower env (Constant4 (Vec4 1 1 1 1))
      InstancedStream _ _ color -> load (vec4Type types) color
  Homogeneous xyz -> case env of
    VertexEnv _ vars -> do
      xyzValue <- lower env xyz
      value OpCompositeConstruct (vec4Type types) [xyzValue, oneId vars]
  Transform xyzw -> case env of
    VertexEnv _ vars -> do
      vector <- lower env xyzw
      matrixAddress <- value OpAccessChain (matrixPointerId vars) [pushId vars, zeroId vars]
      matrix <- load (mat4Type types) matrixAddress
      value OpMatrixTimesVector (vec4Type types) [matrix, vector]
  InterpolatedUV -> case env of FragmentEnv _ vars -> load (vec2Type types) (fragmentUVId vars)
  InterpolatedColor -> case env of FragmentEnv _ vars -> load (vec4Type types) (fragmentColorId vars)
  InterpolatedPosition -> case env of FragmentEnv _ vars -> load (vec3Type types) (fragmentPositionId vars)
  InterpolatedNormal -> case env of FragmentEnv _ vars -> load (vec3Type types) (fragmentNormalId vars)
  InterpolatedSourcePosition -> case env of FragmentEnv _ vars -> load (vec3Type types) (fragmentSourcePositionId vars)
  InterpolatedSourceNormal -> case env of FragmentEnv _ vars -> load (vec3Type types) (fragmentSourceNormalId vars)
  InterpolatedExtra -> case env of FragmentEnv _ vars -> load (vec2Type types) (fragmentExtraId vars)
  Derivative axis shape operand -> do
    requireCapability capabilityDerivativeControl
    argument <- lower env operand
    value (case axis of ScreenX -> OpDPdxFine; ScreenY -> OpDPdyFine) (floatShapeType types shape) [argument]
  FragmentCoordinates -> case env of FragmentEnv _ vars -> load (vec4Type types) (fragmentCoordinateId vars)
  FrontFacing -> case env of FragmentEnv _ vars -> load (boolType types) (fragmentFrontFacingId vars)
  SampleTexture coords -> lower env (SampleAt Texture0 coords)
  SampleAt slot coords -> do
    let sampledTy = case env of
          VertexEnv _ vars -> vertexSampledType vars
          FragmentEnv _ vars -> sampledImageType vars
    sampler <- samplerFor sampledTy slot
    uv <- lower env coords
    sampled <- load sampledTy sampler
    mip <- declare OpConstant (floatType types) [castFloatToWord32 0]
    value OpImageSampleExplicitLod (vec4Type types) [sampled, uv, imageOperandLod, mip]
  Multiply first second -> lower env (FloatBinary Float4 MultiplyF first second)
  Constant4 (Vec4 x y z w) -> do
    scalars <- traverse (declare OpConstant (floatType types) . pure . castFloatToWord32) [x, y, z, w]
    value OpCompositeConstruct (vec4Type types) scalars
  FloatLiteral literal -> declare OpConstant (floatType types) [castFloatToWord32 literal]
  MakeVec2 x y -> construct (vec2Type types) [x, y]
  MakeVec3 x y z -> construct (vec3Type types) [x, y, z]
  MakeVec4 x y z w -> construct (vec4Type types) [x, y, z, w]
  MakeMat4 x y z w -> construct (mat4Type types) [x, y, z, w]
  Component component exprValue -> do
    vector <- lower env exprValue
    value OpCompositeExtract (floatType types) [vector, componentIndex component]
  FloatUnary shape op operand -> do
    argument <- lower env operand
    let ty = floatShapeType types shape
    case op of
      NegateF -> value OpFNegate ty [argument]
      Absolute -> extended ty glslFAbs [argument]
      FloorF -> extended ty glslFloor [argument]
      Fract -> extended ty glslFract [argument]
      Sin -> extended ty glslSin [argument]
      Cos -> extended ty glslCos [argument]
      Asin -> extended ty glslAsin [argument]
      Exp -> extended ty glslExp [argument]
      Exp2 -> extended ty glslExp2 [argument]
      Sqrt -> extended ty glslSqrt [argument]
      InverseSqrt -> extended ty glslInverseSqrt [argument]
  FloatBinary shape op lhs rhs -> do
    arguments <- traverse (lower env) [lhs, rhs]
    let ty = floatShapeType types shape
    case op of
      AddF -> value OpFAdd ty arguments
      SubtractF -> value OpFSub ty arguments
      MultiplyF -> value OpFMul ty arguments
      DivideF -> value OpFDiv ty arguments
      Minimum -> extended ty glslFMin arguments
      Maximum -> extended ty glslFMax arguments
      Power -> extended ty glslPow arguments
      Step -> extended ty glslStep arguments
      Atan2 -> extended ty glslAtan2 arguments
  FloatTernary shape op a b c -> do
    arguments <- traverse (lower env) [a, b, c]
    extended (floatShapeType types shape)
      (case op of Clamp -> glslFClamp; Mix -> glslFMix; Smoothstep -> glslSmoothStep) arguments
  ScaleVector shape vector scalar -> do
    vectorId <- lower env vector
    scalarId <- lower env scalar
    value OpVectorTimesScalar (floatShapeType types (vectorFloats shape)) [vectorId, scalarId]
  Dot _ lhs rhs -> traverse (lower env) [lhs, rhs] >>= value OpDot (floatType types)
  Length _ vector -> do
    vectorId <- lower env vector
    extended (floatType types) glslLength [vectorId]
  Normalize shape vector -> do
    vectorId <- lower env vector
    extended (floatShapeType types (vectorFloats shape)) glslNormalize [vectorId]
  Cross lhs rhs -> traverse (lower env) [lhs, rhs] >>= extended (vec3Type types) glslCross
  MatrixVector matrix vector -> do
    matrixId <- lower env matrix
    vectorId <- lower env vector
    value OpMatrixTimesVector (vec4Type types) [matrixId, vectorId]
  Compare op lhs rhs -> do
    arguments <- traverse (lower env) [lhs, rhs]
    value (case op of Less -> OpFOrdLessThan; LessEqual -> OpFOrdLessThanEqual;
                       Greater -> OpFOrdGreaterThan; EqualF -> OpFOrdEqual) (boolType types) arguments
  BoolLiteral literal -> declare (if literal then OpConstantTrue else OpConstantFalse) (boolType types) []
  BoolBinary op lhs rhs -> do
    arguments <- traverse (lower env) [lhs, rhs]
    value (case op of Both -> OpLogicalAnd; EitherTrue -> OpLogicalOr) (boolType types) arguments
  BoolNot condition -> do
    identifier <- lower env condition
    value OpLogicalNot (boolType types) [identifier]
  Select shape condition yes no -> do
    test <- lower env condition
    operands <- traverse (lower env) [yes, no]
    value OpSelect (floatShapeType types shape) (test : operands)
  If shape condition yes no -> do
    test <- lower env condition
    whenTrue <- newId
    whenFalse <- newId
    merge <- newId
    emit FunctionBody OpSelectionMerge [merge, 0]
    emit FunctionBody OpBranchConditional [test, whenTrue, whenFalse]
    labelBlock whenTrue
    yesValue <- lower env yes
    yesBlock <- currentBlock
    emit FunctionBody OpBranch [merge]
    labelBlock whenFalse
    noValue <- lower env no
    noBlock <- currentBlock
    emit FunctionBody OpBranch [merge]
    labelBlock merge
    value OpPhi (floatShapeType types shape) [yesValue, yesBlock, noValue, noBlock]
  Uniform (UniformSlot slot) -> do
    parameters <- parameterBlock types
    index <- declare OpConstant (unsignedType types) [slot]
    member <- declare OpConstant (unsignedType types) [0]
    vecPointer <- pointerTypeFor storageUniform (vec4Type types)
    address <- value OpAccessChain vecPointer [parameters, member, index]
    load (vec4Type types) address
  UnsignedLiteral literal -> declare OpConstant (unsignedType types) [literal]
  SignedBits operand -> do
    argument <- lower env operand
    signed <- value OpConvertFToS (signedType types) [argument]
    value OpBitcast (unsignedType types) [signed]
  UnsignedFloat operand -> do
    argument <- lower env operand
    value OpConvertUToF (floatType types) [argument]
  CountFromFloat operand -> do
    argument <- lower env operand
    lowerBound <- declare OpConstant (floatType types) [castFloatToWord32 0]
    upperBound <- declare OpConstant (floatType types) [castFloatToWord32 64]
    bounded <- extended (floatType types) glslFClamp [argument, lowerBound, upperBound]
    value OpConvertFToU (unsignedType types) [bounded]
  UnsignedBinary op lhs rhs -> do
    arguments <- traverse (lower env) [lhs, rhs]
    value (case op of MultiplyU -> OpIMul; Xor -> OpBitwiseXor; AndBits -> OpBitwiseAnd;
                       RightShift -> OpShiftRightLogical) (unsignedType types) arguments
  LetExpr local continuation -> do
    identifier <- lower env local
    lower env (continuation (Bound (Emitted identifier)))
  Bound (Emitted identifier) -> pure identifier
  FoldCount shape (LoopLimit bound) requested initial body -> do
    let targetType = floatShapeType types shape
    let uint = unsignedType types
    limit <- declare OpConstant uint [bound]
    count <- lower env requested
    zero <- declare OpConstant uint [0]
    one <- declare OpConstant uint [1]
    initialValue <- lower env initial
    parent <- currentBlock
    loop <- newId
    calculate <- newId
    advance <- newId
    merge <- newId
    nextIndex <- newId
    nextValue <- newId
    emit FunctionBody OpBranch [loop]
    labelBlock loop
    index <- value OpPhi uint [zero, parent, nextIndex, advance]
    accumulator <- value OpPhi targetType [initialValue, parent, nextValue, advance]
    belowLimit <- value OpULessThan (boolType types) [index, limit]
    belowCount <- value OpULessThan (boolType types) [index, count]
    test <- value OpLogicalAnd (boolType types) [belowLimit, belowCount]
    emit FunctionBody OpLoopMerge [merge, advance, 0]
    emit FunctionBody OpBranchConditional [test, calculate, merge]
    labelBlock calculate
    result <- lower env (body (Bound (Emitted index)) (Bound (Emitted accumulator)))
    emit FunctionBody OpBranch [advance]
    labelBlock advance
    emit FunctionBody OpIAdd [uint, nextIndex, index, one]
    emit FunctionBody OpCopyObject [targetType, nextValue, result]
    emit FunctionBody OpBranch [loop]
    labelBlock merge
    pure accumulator
  where
    types = case env of
      VertexEnv t _ -> t
      FragmentEnv t _ -> t
    load ty identifier = value OpLoad ty [identifier]
    construct :: Id -> [Expr Emitted stage item] -> Emit Id
    construct ty operands = traverse (lower env) operands >>= value OpCompositeConstruct ty
    extended ty instruction arguments = value OpExtInst ty (extLibrary types : instruction : arguments)

floatShapeType :: Types -> FloatShape a -> Id
floatShapeType types shape = case shape of
  Float1 -> floatType types
  Float2 -> vec2Type types
  Float3 -> vec3Type types
  Float4 -> vec4Type types

samplerFor :: Id -> TextureSlot -> Emit Id
samplerFor sampledType slot = Emit $ \state ->
  case lookup slot (samplerVariables state) of
    Just identifier -> (identifier, state)
    Nothing ->
      let Emit defineSampler = do
            sampler <- variable storageUniformConstant sampledType
            decorate sampler decorateBinding [textureBinding]
            decorate sampler decorateDescriptorSet [textureSlotIndex slot]
            pure sampler
          (identifier, updated) = defineSampler state
      in (identifier, updated { samplerVariables = (slot, identifier) : samplerVariables updated })

parameterBlock :: Types -> Emit Id
parameterBlock types = Emit $ \state ->
  case parameterVariable state of
    Just identifier -> (identifier, state)
    Nothing ->
      let Emit defineParameter = do
            count <- declare OpConstant (unsignedType types) [uniformSlots]
            array <- define OpTypeArray [vec4Type types, count]
            decorate array decorateArrayStride [transformColumnStride]
            block <- define OpTypeStruct [array]
            decorate block decorateBlock []
            emit Annotation OpMemberDecorate [block, 0, decorateOffset, 0]
            parameters <- variable storageUniform block
            decorate parameters decorateBinding [0]
            decorate parameters decorateDescriptorSet [uniformDescriptorSet]
            pure parameters
          (identifier, updated) = defineParameter state
      in (identifier, updated { parameterVariable = Just identifier })

compile :: Word32 -> [Word32] -> (Id -> Emit ()) -> [Word32]
compile stage executionModes makeModule =
  header ++ concatMap encode (preamble ++ body)
  where
    Emit assemble = do
      entry <- newId
      makeModule entry
      pure entry
    (entryPoint, final) = assemble (ModuleState 1 [] [] [] [] [] [] [] [] Nothing 0)
    header = [0x07230203, 0x00010400, 0, nextId final, 0]
    preamble = [Instruction OpCapability [capability] | capability <- capabilityShader : reverse (extraCapabilities final)] ++
      reverse (extensionImports final) ++
      [ Instruction OpMemoryModel [logicalAddressing, memoryGLSL450]
      , Instruction OpEntryPoint ([stage, entryPoint] ++ encodeString "main" ++ reverse (globalVariables final))
      ] ++ [Instruction OpExecutionMode [entryPoint, mode] | mode <- executionModes]
    body = reverse (annotations final) ++ reverse (globals final) ++ reverse (functionBody final)
    encode (Instruction opcode operands) =
      ((fromIntegral (1 + length operands) `shiftL` 16) .|. opcode) : operands

encodeString :: String -> [Word32]
encodeString = pack . (++ [0]) . map (fromIntegral . ord)
  where
    pack [] = []
    pack bytes =
      let (prefix, rest) = splitAt 4 bytes
          word = foldr (.|.) 0 (zipWith shiftL prefix [0, 8, 16, 24])
      in word : pack rest

-- Numeric assignments from the Khronos SPIR-V unified core grammar.
-- https://github.com/KhronosGroup/SPIRV-Headers/blob/main/include/spirv/unified1/spirv.core.grammar.json
pattern OpCapability :: Word32
pattern OpCapability = 17

pattern OpMemoryModel :: Word32
pattern OpMemoryModel = 14

pattern OpEntryPoint :: Word32
pattern OpEntryPoint = 15

pattern OpExecutionMode :: Word32
pattern OpExecutionMode = 16

pattern OpDecorate :: Word32
pattern OpDecorate = 71

pattern OpMemberDecorate :: Word32
pattern OpMemberDecorate = 72

pattern OpTypeVoid :: Word32
pattern OpTypeVoid = 19

pattern OpTypeInt :: Word32
pattern OpTypeInt = 21

pattern OpTypeFloat :: Word32
pattern OpTypeFloat = 22

pattern OpTypeVector :: Word32
pattern OpTypeVector = 23

pattern OpTypeMatrix :: Word32
pattern OpTypeMatrix = 24

pattern OpTypePointer :: Word32
pattern OpTypePointer = 32

pattern OpTypeFunction :: Word32
pattern OpTypeFunction = 33

pattern OpTypeStruct :: Word32
pattern OpTypeStruct = 30

pattern OpTypeImage :: Word32
pattern OpTypeImage = 25

pattern OpTypeSampledImage :: Word32
pattern OpTypeSampledImage = 27

pattern OpVariable :: Word32
pattern OpVariable = 59

pattern OpConstant :: Word32
pattern OpConstant = 43

pattern OpConstantTrue, OpConstantFalse :: Word32
pattern OpConstantTrue = 41
pattern OpConstantFalse = 42

pattern OpFunction :: Word32
pattern OpFunction = 54

pattern OpLabel :: Word32
pattern OpLabel = 248

pattern OpReturn :: Word32
pattern OpReturn = 253

pattern OpKill :: Word32
pattern OpKill = 252

pattern OpDPdxFine, OpDPdyFine :: Word32
pattern OpDPdxFine = 210
pattern OpDPdyFine = 211

pattern OpFunctionEnd :: Word32
pattern OpFunctionEnd = 56

pattern OpLoad :: Word32
pattern OpLoad = 61

pattern OpStore :: Word32
pattern OpStore = 62

pattern OpCompositeConstruct :: Word32
pattern OpCompositeConstruct = 80

pattern OpAccessChain :: Word32
pattern OpAccessChain = 65

pattern OpMatrixTimesVector :: Word32
pattern OpMatrixTimesVector = 145

imageOperandLod :: Word32
imageOperandLod = 2

pattern OpFMul :: Word32
pattern OpFMul = 133

storageInput :: Word32
storageInput = 1

storageOutput :: Word32
storageOutput = 3

storageUniformConstant :: Word32
storageUniformConstant = 0

storagePushConstant :: Word32
storagePushConstant = 9

stageVertex :: Word32
stageVertex = 0

stageFragment :: Word32
stageFragment = 4

originUpperLeft :: Word32
originUpperLeft = 7

capabilityShader, capabilityDerivativeControl :: Word32
capabilityShader = 1
capabilityDerivativeControl = 51

logicalAddressing :: Word32
logicalAddressing = 0

memoryGLSL450 :: Word32
memoryGLSL450 = 1

decorateLocation :: Word32
decorateLocation = 30

decorateBuiltIn :: Word32
decorateBuiltIn = 11

decorateBinding :: Word32
decorateBinding = 33

decorateDescriptorSet :: Word32
decorateDescriptorSet = 34

decorateBlock :: Word32
decorateBlock = 2

decorateColMajor :: Word32
decorateColMajor = 5

decorateMatrixStride :: Word32
decorateMatrixStride = 7

decorateOffset :: Word32
decorateOffset = 35

builtInPosition :: Word32
builtInPosition = 0

image2D :: Word32
image2D = 1

unknownImageFormat :: Word32
unknownImageFormat = 0
pattern OpExtInstImport :: Word32
pattern OpExtInstImport = 11

pattern OpExtInst :: Word32
pattern OpExtInst = 12

pattern OpTypeBool :: Word32
pattern OpTypeBool = 20

pattern OpTypeArray :: Word32
pattern OpTypeArray = 28

pattern OpCompositeExtract :: Word32
pattern OpCompositeExtract = 81

pattern OpFAdd :: Word32
pattern OpFAdd = 129

pattern OpFSub :: Word32
pattern OpFSub = 131

pattern OpFDiv :: Word32
pattern OpFDiv = 136

pattern OpFNegate :: Word32
pattern OpFNegate = 127

pattern OpDot :: Word32
pattern OpDot = 148

pattern OpVectorTimesScalar :: Word32
pattern OpVectorTimesScalar = 142

pattern OpSelect :: Word32
pattern OpSelect = 169

pattern OpFOrdLessThan :: Word32
pattern OpFOrdLessThan = 184

pattern OpFOrdLessThanEqual :: Word32
pattern OpFOrdLessThanEqual = 188

pattern OpFOrdGreaterThan :: Word32
pattern OpFOrdGreaterThan = 186

pattern OpFOrdEqual :: Word32
pattern OpFOrdEqual = 180

pattern OpLogicalAnd :: Word32
pattern OpLogicalAnd = 167

pattern OpLogicalOr :: Word32
pattern OpLogicalOr = 166

pattern OpLogicalNot :: Word32
pattern OpLogicalNot = 168

pattern OpConvertFToS :: Word32
pattern OpConvertFToS = 110

pattern OpConvertUToF :: Word32
pattern OpConvertUToF = 112

pattern OpBitcast :: Word32
pattern OpBitcast = 124

pattern OpIMul :: Word32
pattern OpIMul = 132

pattern OpBitwiseXor :: Word32
pattern OpBitwiseXor = 198

pattern OpBitwiseAnd :: Word32
pattern OpBitwiseAnd = 199

pattern OpShiftRightLogical :: Word32
pattern OpShiftRightLogical = 194

glslFAbs :: Word32
glslFAbs = 4

glslFloor :: Word32
glslFloor = 8

glslFract :: Word32
glslFract = 10

glslSin :: Word32
glslSin = 13

glslCos :: Word32
glslCos = 14

glslExp :: Word32
glslExp = 27

glslExp2 :: Word32
glslExp2 = 29

glslSqrt :: Word32
glslSqrt = 31

glslInverseSqrt :: Word32
glslInverseSqrt = 32

glslFMin :: Word32
glslFMin = 37

glslFMax :: Word32
glslFMax = 40

glslPow :: Word32
glslPow = 26

glslStep :: Word32
glslStep = 48

glslFClamp :: Word32
glslFClamp = 43

glslFMix :: Word32
glslFMix = 46

glslSmoothStep :: Word32
glslSmoothStep = 49

glslLength :: Word32
glslLength = 66

glslNormalize :: Word32
glslNormalize = 69

storageUniform :: Word32
storageUniform = 2

decorateArrayStride :: Word32
decorateArrayStride = 6

builtInFragmentCoordinate :: Word32
builtInFragmentCoordinate = 15

builtInFrontFacing :: Word32
builtInFrontFacing = 17

pattern OpImageSampleExplicitLod :: Word32
pattern OpImageSampleExplicitLod = 88

pattern OpSelectionMerge :: Word32
pattern OpSelectionMerge = 247

pattern OpBranchConditional :: Word32
pattern OpBranchConditional = 250

pattern OpBranch :: Word32
pattern OpBranch = 249

pattern OpPhi :: Word32
pattern OpPhi = 245

pattern OpLoopMerge :: Word32
pattern OpLoopMerge = 246

pattern OpULessThan :: Word32
pattern OpULessThan = 176

pattern OpIAdd :: Word32
pattern OpIAdd = 128

pattern OpCopyObject :: Word32
pattern OpCopyObject = 83

pattern OpConvertFToU :: Word32
pattern OpConvertFToU = 109

glslAsin :: Word32
glslAsin = 16

glslAtan2 :: Word32
glslAtan2 = 25

glslCross :: Word32
glslCross = 68

module GameIO.Graphics.Plan
  ( Color(..)
  , Vertex(..)
  , vertexValues
  , vertexExtra
  , InstanceData(..)
  , Vec2(..)
  , Vec3(..)
  , Vec4(..)
  , Mat4
  , identityMat4
  , mat4FromColumns
  , mat4Columns
  , transformVec4
  , multiplyMat4
  , translateMat4
  , scaleMat4
  , rotateYMat4
  , perspectiveMat4
  , orthographicMat4
  , lookAtMat4
  , pixelMat4
  , MeshData
  , meshData
  , meshVertices
  , meshIndices
  , meshIndexCount
  , PlanError(..)
  , VertexAttribute(..)
  , vertexLayout
  , vertexStride
  , positionLocation
  , uvLocation
  , colorLocation
  , normalLocation
  , vertexExtraLocation
  , instanceRootLocation
  , instanceParametersLocation
  , instanceColorLocation
  , instanceRootOffset
  , instanceParametersOffset
  , instanceColorOffset
  , instanceTagOffset
  , instanceStride
  , cornerStride
  , varyingUVLocation
  , varyingColorLocation
  , varyingPositionLocation
  , varyingNormalLocation
  , varyingSourcePositionLocation
  , varyingSourceNormalLocation
  , varyingExtraLocation
  , transformOffset
  , transformColumnStride
  , transformBytes
  , ImageUse(..)
  , ImageTransition(..)
  , FrameStep(..)
  , planFrame
  , planTextureUpload
  , PassInput(..)
  , ImageVersion(..)
  , TargetPass(..)
  , PassError(..)
  , planPasses
  ) where

import Data.List (find)
import qualified Data.List as List
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word8, Word32, Word64)

data Color = Color !Float !Float !Float !Float deriving (Eq, Show)
data Vertex
  = Vertex !Float !Float !Float !Float !Float !Color
  | VertexWithNormal !Vec3 !Vec2 !Color !Vec3
  | VertexWithDetails !Vec3 !Vec2 !Color !Vec3 !Vec2
  deriving (Eq, Show)

-- The tag belongs to the producer's stable record order. The input layout
-- reads only the two vectors and the four normalized color bytes.
data InstanceData = InstanceData !Vec4 !Vec4 !(Word8, Word8, Word8, Word8) !Word32 deriving (Eq, Show)

data Vec2 = Vec2 !Float !Float deriving (Eq, Show)
data Vec3 = Vec3 !Float !Float !Float deriving (Eq, Show)
data Vec4 = Vec4 !Float !Float !Float !Float deriving (Eq, Show)

vertexValues :: Vertex -> (Vec3, Vec2, Color, Vec3)
vertexValues vertex = case vertex of
  Vertex x y z u v color -> (Vec3 x y z, Vec2 u v, color, Vec3 0 1 0)
  VertexWithNormal position uv color normal -> (position, uv, color, normal)
  VertexWithDetails position uv color normal _ -> (position, uv, color, normal)

vertexExtra :: Vertex -> Vec2
vertexExtra vertex = case vertex of
  VertexWithDetails _ _ _ _ extra -> extra
  _ -> Vec2 0 0

-- Column vectors multiply on the right. Matrix bytes keep this column order.
data Mat4 = Mat4 !Vec4 !Vec4 !Vec4 !Vec4 deriving (Eq, Show)

data PlanError
  = EmptyMesh
  | MeshTooLarge
  | IncompleteTriangles !Int
  | InvalidVertex !Int
  | IndexOutsideMesh !Word32 !Int
  | InvalidProjection
  | DegenerateCamera
  deriving (Eq, Show)

identityMat4 :: Mat4
identityMat4 = Mat4 (Vec4 1 0 0 0) (Vec4 0 1 0 0) (Vec4 0 0 1 0) (Vec4 0 0 0 1)

mat4FromColumns :: Vec4 -> Vec4 -> Vec4 -> Vec4 -> Mat4
mat4FromColumns = Mat4

mat4Columns :: Mat4 -> [Vec4]
mat4Columns (Mat4 c0 c1 c2 c3) = [c0, c1, c2, c3]

transformVec4 :: Mat4 -> Vec4 -> Vec4
transformVec4 (Mat4 c0 c1 c2 c3) (Vec4 x y z w) =
  add4 (add4 (scale4 x c0) (scale4 y c1)) (add4 (scale4 z c2) (scale4 w c3))

multiplyMat4 :: Mat4 -> Mat4 -> Mat4
multiplyMat4 left (Mat4 c0 c1 c2 c3) = Mat4
  (transformVec4 left c0) (transformVec4 left c1)
  (transformVec4 left c2) (transformVec4 left c3)

translateMat4 :: Vec3 -> Mat4
translateMat4 (Vec3 x y z) = Mat4
  (Vec4 1 0 0 0) (Vec4 0 1 0 0) (Vec4 0 0 1 0) (Vec4 x y z 1)

scaleMat4 :: Vec3 -> Mat4
scaleMat4 (Vec3 x y z) = Mat4
  (Vec4 x 0 0 0) (Vec4 0 y 0 0) (Vec4 0 0 z 0) (Vec4 0 0 0 1)

rotateYMat4 :: Float -> Mat4
rotateYMat4 angle = Mat4
  (Vec4 cosine 0 (-sine) 0) (Vec4 0 1 0 0) (Vec4 sine 0 cosine 0) (Vec4 0 0 0 1)
  where
    cosine = cos angle
    sine = sin angle

-- Right-handed view space looks along -Z. Vulkan depth is [0,1].
-- Negating view-space Y keeps +Y up on a positive-height Vulkan viewport.
perspectiveMat4 :: Float -> Float -> Float -> Float -> Either PlanError Mat4
perspectiveMat4 fovy aspect near far
  | not (all finite [fovy, aspect, near, far]) = Left InvalidProjection
  | fovy <= 0 || fovy >= pi || aspect <= 0 || near <= 0 || far <= near = Left InvalidProjection
  | all finite [horizontal, vertical, depth, shift] = Right $ Mat4
      (Vec4 horizontal 0 0 0)
      (Vec4 0 (-vertical) 0 0)
      (Vec4 0 0 depth (-1))
      (Vec4 0 0 shift 0)
  | otherwise = Left InvalidProjection
  where
    vertical = recip (tan (fovy / 2))
    horizontal = vertical / aspect
    depth = far / (near - far)
    shift = near * depth

orthographicMat4 :: Float -> Float -> Float -> Float -> Either PlanError Mat4
orthographicMat4 height aspect near far
  | not (all finite [height, aspect, near, far]) = Left InvalidProjection
  | height <= 0 || aspect <= 0 || near < 0 || far <= near = Left InvalidProjection
  | all finite [horizontal, vertical, depth, shift] = Right $ Mat4
      (Vec4 horizontal 0 0 0)
      (Vec4 0 (-vertical) 0 0)
      (Vec4 0 0 depth 0)
      (Vec4 0 0 shift 1)
  | otherwise = Left InvalidProjection
  where
    vertical = 2 / height
    horizontal = vertical / aspect
    depth = recip (near - far)
    shift = near * depth

lookAtMat4 :: Vec3 -> Vec3 -> Vec3 -> Either PlanError Mat4
lookAtMat4 eye target up = do
  forward <- normalize3 (subtract3 target eye)
  right <- normalize3 (cross3 forward up)
  let vertical = cross3 right forward
      back = scale3 (-1) forward
      Vec3 rx ry rz = right
      Vec3 ux uy uz = vertical
      Vec3 bx by bz = back
      tx = -(dot3 right eye)
      ty = -(dot3 vertical eye)
      tz = -(dot3 back eye)
  if all finite [tx, ty, tz]
    then Right $ Mat4 (Vec4 rx ux bx 0) (Vec4 ry uy by 0)
      (Vec4 rz uz bz 0) (Vec4 tx ty tz 1)
    else Left DegenerateCamera

pixelMat4 :: Int -> Int -> Either PlanError Mat4
pixelMat4 width height
  | width <= 0 || height <= 0 = Left InvalidProjection
  | otherwise = Right $ Mat4 (Vec4 (2 / fromIntegral width) 0 0 0)
      (Vec4 0 (2 / fromIntegral height) 0 0) (Vec4 0 0 1 0) (Vec4 (-1) (-1) 0 1)

data MeshData = MeshData ![Vertex] ![Word32] !Word32 deriving (Eq, Show)

meshData :: [Vertex] -> [Word32] -> Either PlanError MeshData
meshData vertices indices
  | vertexCount == 0 || indexCount == 0 = Left EmptyMesh
  | toInteger vertexCount > largest || toInteger indexCount > largest = Left MeshTooLarge
  | indexCount `mod` 3 /= 0 = Left (IncompleteTriangles indexCount)
  | Just (index, _) <- find (not . finiteVertex . snd) (zip [0..] vertices) =
      Left (InvalidVertex index)
  | Just index <- find (\i -> fromIntegral i >= vertexCount) indices =
      Left (IndexOutsideMesh index vertexCount)
  | otherwise = Right (MeshData vertices indices (fromIntegral indexCount))
  where
    vertexCount = length vertices
    indexCount = length indices
    largest = toInteger (maxBound :: Word32)

meshVertices :: MeshData -> [Vertex]
meshVertices (MeshData vertices _ _) = vertices

meshIndices :: MeshData -> [Word32]
meshIndices (MeshData _ indices _) = indices

meshIndexCount :: MeshData -> Word32
meshIndexCount (MeshData _ _ count) = count

data VertexAttribute = VertexAttribute
  { attributeLocation :: !Word32
  , attributeComponents :: !Int
  , attributeOffset :: !Int
  } deriving (Eq, Show)

positionLocation, uvLocation, colorLocation, normalLocation, vertexExtraLocation :: Word32
positionLocation = 0
uvLocation = 1
colorLocation = 2
normalLocation = 3
vertexExtraLocation = 4

instanceRootLocation, instanceParametersLocation, instanceColorLocation :: Word32
instanceRootLocation = 5
instanceParametersLocation = 6
instanceColorLocation = 7

varyingUVLocation, varyingColorLocation, varyingPositionLocation, varyingNormalLocation :: Word32
varyingUVLocation = 0
varyingColorLocation = 1
varyingPositionLocation = 2
varyingNormalLocation = 3

varyingSourcePositionLocation, varyingSourceNormalLocation, varyingExtraLocation :: Word32
varyingSourcePositionLocation = 4
varyingSourceNormalLocation = 5
varyingExtraLocation = 6

vertexLayout :: [VertexAttribute]
vertexLayout = snd $ List.foldl' append (0, [])
  [(positionLocation, 3), (uvLocation, 2), (colorLocation, 4), (normalLocation, 3), (vertexExtraLocation, 2)]
  where
    append (offset, attributes) (location, components) =
      (offset + components * floatBytes, attributes ++ [VertexAttribute location components offset])

vertexStride :: Int
vertexStride = sum [floatBytes * attributeComponents attribute | attribute <- vertexLayout]

cornerStride, instanceRootOffset, instanceParametersOffset, instanceColorOffset, instanceTagOffset, instanceStride :: Int
cornerStride = 3 * floatBytes
instanceRootOffset = 0
instanceParametersOffset = instanceRootOffset + 4 * floatBytes
instanceColorOffset = instanceParametersOffset + 4 * floatBytes
instanceTagOffset = instanceColorOffset + 4
instanceStride = instanceTagOffset + 4

floatBytes :: Int
floatBytes = 4

transformOffset, transformColumnStride, transformBytes :: Word32
transformOffset = 0
transformColumnStride = 4 * fromIntegral floatBytes
transformBytes = 4 * transformColumnStride

data ImageUse
  = Discarded
  | ColorTarget
  | DepthTarget
  | CopySource
  | CopyDestination
  | ShaderSampled
  | Presentation
  deriving (Eq, Show)

data ImageTransition = ImageTransition !ImageUse !ImageUse deriving (Eq, Show)

data FrameStep
  = TransitionColor !ImageTransition
  | TransitionDepth !ImageTransition
  | DrawGeometry
  | CopyColorToHost
  deriving (Eq, Show)

planFrame :: Bool -> [FrameStep]
planFrame readBack =
  [ TransitionColor (ImageTransition Discarded ColorTarget)
  , TransitionDepth (ImageTransition Discarded DepthTarget)
  , DrawGeometry
  ] ++ if readBack
    then [ TransitionColor (ImageTransition ColorTarget CopySource)
         , CopyColorToHost
         , TransitionColor (ImageTransition CopySource Presentation)
         ]
    else [TransitionColor (ImageTransition ColorTarget Presentation)]

planTextureUpload :: (ImageTransition, ImageTransition)
planTextureUpload = (ImageTransition Discarded CopyDestination, ImageTransition CopyDestination ShaderSampled)

data PassInput = PassInput !Int ![Int] deriving (Eq, Show)
data ImageVersion = ImageVersion !Int !Word64 deriving (Eq, Show)

data TargetPass = TargetPass
  { targetPrevious :: !(Maybe ImageVersion)
  , targetOutput :: !ImageVersion
  , targetInputs :: ![ImageVersion]
  } deriving (Eq, Show)

data PassError
  = SampleBeforeWrite !Int
  | SampleOwnAttachment !Int
  | ExhaustedImageVersions !Int
  deriving (Eq, Show)

planPasses :: IntMap.IntMap Word64 -> [PassInput] -> [Int]
           -> Either PassError ([TargetPass], [ImageVersion])
planPasses existing passes presentReads = go existing passes
  where
    go versions [] = (,) [] <$> traverse (readVersion versions) presentReads
    go versions (PassInput output inputs : rest)
      | output `elem` inputs = Left (SampleOwnAttachment output)
      | otherwise = do
          let previous = IntMap.lookup output versions
          next <- case previous of
            Just old | old == maxBound -> Left (ExhaustedImageVersions output)
            Just old -> Right (old + 1)
            Nothing -> Right 1
          readInputs <- traverse (readVersion versions) inputs
          (following, final) <- go (IntMap.insert output next versions) rest
          pure (TargetPass (ImageVersion output <$> previous) (ImageVersion output next) readInputs : following, final)
    readVersion versions target =
      maybe (Left (SampleBeforeWrite target)) (Right . ImageVersion target) (IntMap.lookup target versions)

finiteVertex :: Vertex -> Bool
finiteVertex vertex = case vertexValues vertex of
  (Vec3 x y z, Vec2 u v, Color r g b a, Vec3 nx ny nz) ->
    let Vec2 first second = vertexExtra vertex
    in all finite [x, y, z, u, v, r, g, b, a, nx, ny, nz, first, second]

finite :: Float -> Bool
finite x = not (isNaN x || isInfinite x)

add4 :: Vec4 -> Vec4 -> Vec4
add4 (Vec4 x y z w) (Vec4 a b c d) = Vec4 (x + a) (y + b) (z + c) (w + d)

scale4 :: Float -> Vec4 -> Vec4
scale4 amount (Vec4 x y z w) = Vec4 (amount * x) (amount * y) (amount * z) (amount * w)

subtract3 :: Vec3 -> Vec3 -> Vec3
subtract3 (Vec3 x y z) (Vec3 a b c) = Vec3 (x - a) (y - b) (z - c)

scale3 :: Float -> Vec3 -> Vec3
scale3 amount (Vec3 x y z) = Vec3 (amount * x) (amount * y) (amount * z)

dot3 :: Vec3 -> Vec3 -> Float
dot3 (Vec3 x y z) (Vec3 a b c) = x * a + y * b + z * c

cross3 :: Vec3 -> Vec3 -> Vec3
cross3 (Vec3 x y z) (Vec3 a b c) = Vec3 (y * c - z * b) (z * a - x * c) (x * b - y * a)

normalize3 :: Vec3 -> Either PlanError Vec3
normalize3 (Vec3 x y z)
  | not (finite scale) || scale <= 0 = Left DegenerateCamera
  | otherwise =
      let scaled = Vec3 (x / scale) (y / scale) (z / scale)
      in Right (scale3 (recip (sqrt (dot3 scaled scaled))) scaled)
  where
    scale = max (abs x) (max (abs y) (abs z))

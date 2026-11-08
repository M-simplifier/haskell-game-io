-- Run from the repository with ghc -fno-code -isrc tests/compile-fail/ShaderEscapedBinding.hs.
-- Expect a rigid local type mismatch when subshader captures the open binding.
module ShaderEscapedBinding where

import GameIO.Graphics.Plan
import GameIO.Graphics.Shader

escaped :: FragmentShader
escaped = FragmentShader $ letExpr (FloatLiteral 1) $ \local ->
  let subshader = FragmentShader (MakeVec4 local 0 0 1)
      red = case evaluateFragment subshader (\_ _ -> white) (const (Vec4 0 0 0 0)) quad of
        Just (Color r _ _ _) -> r
        Nothing -> 0
  in Constant4 (Vec4 red 0 0 1)
  where
    white = Color 1 1 1 1
    point = FragmentSample (Vec2 0 0) white (Vec3 0 0 0) (Vec3 0 1 0)
      (Vec3 0 0 0) (Vec3 0 1 0) (Vec2 0 0) (Vec4 0 0 0 1) True
    quad = FragmentQuad point point point point

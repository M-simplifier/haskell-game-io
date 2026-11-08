module GameIO.Text.Render
  ( AtlasTexture
  , createAtlasTexture
  , destroyAtlasTexture
  , withAtlasTexture
  , textureAtlas
  , atlasTextBatch
  ) where

import Control.Exception (bracket)
import GameIO.Graphics
import GameIO.Text
import GameIO.Text.Atlas

data AtlasTexture = AtlasTexture !GlyphAtlas !Texture

createAtlasTexture :: Renderer -> GlyphAtlas -> IO AtlasTexture
createAtlasTexture renderer atlas =
  let (width, height) = atlasSize atlas
  in AtlasTexture atlas <$> createTextureRGBA8With renderer Linear width height (atlasRGBA atlas)

destroyAtlasTexture :: Renderer -> AtlasTexture -> IO ()
destroyAtlasTexture renderer (AtlasTexture _ texture) = destroyTexture renderer texture

withAtlasTexture :: Renderer -> GlyphAtlas -> (AtlasTexture -> IO a) -> IO a
withAtlasTexture renderer atlas = bracket (createAtlasTexture renderer atlas) (destroyAtlasTexture renderer)

textureAtlas :: AtlasTexture -> GlyphAtlas
textureAtlas (AtlasTexture atlas _) = atlas

-- | The glyph run is measured in source atlas pixels. Scale its advance
-- by the same factor as this batch to obtain client pixels.
atlasTextBatch :: AtlasTexture -> Double -> Double -> Double -> Double -> Color -> String -> Either AtlasError (GlyphRun, OverlayBatch)
atlasTextBatch (AtlasTexture atlas texture) left baseline scale spacing color label = do
  (run, quads) <- atlasQuadsSpaced atlas 0 0 spacing label
  pure (run, OverlayBatch (Just texture) (concatMap vertices quads))
  where
    vertices (TextQuad (l,t,r,b) (u0,v0,u1,v1)) =
      let tl = vertex l t u0 v0
          tr = vertex r t u1 v0
          br = vertex r b u1 v1
          bl = vertex l b u0 v1
      in [tl,tr,br,tl,br,bl]
    vertex x y u v = Vertex (realToFrac (left + x * scale)) (realToFrac (baseline + y * scale)) 0 u v color

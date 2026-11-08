module Main (main) where

import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8')
import GameIO.Text
import GameIO.Text.Atlas
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    [path, faceNumber, cataloguePath, outputPath] -> do
      face <- maybe (fail "FACE must be an integer") pure (readMaybe faceNumber)
      font <- BS.readFile path >>= success . decodeFontFace face
      characters <- BS.readFile cataloguePath >>= success . decodeUtf8'
      let catalogue = Set.toList (Set.fromList (Text.unpack characters))
      size <- maybe (fail "invalid probe size") pure (pixelSize 36)
      admitted <- traverse (success . lookupGlyph font) catalogue
      let missing = [character | (character, Nothing) <- zip catalogue admitted]
      atlas <- success (buildAtlas 1024 font size catalogue)
      let (width, height) = atlasSize atlas
          rgba = atlasRGBA atlas
      unless (BS.length rgba == width * height * 4) (fail "atlas byte count")
      putStrLn ("unitsPerEm=" ++ show (fontUnitsPerEm font))
      putStrLn ("catalogue=" ++ show (length catalogue) ++ ", missing=" ++ show (length missing))
      forM_ missing (putStrLn . show)
      let sample = take 160 catalogue
      (line, placed) <- success (atlasQuads atlas 0 0 sample)
      alternate <- success (buildAtlas 768 font size sample)
      (repacked, alternatePlaced) <- success (atlasQuads alternate 0 0 sample)
      unless (runGlyphs line == runGlyphs repacked && runAdvance line == runAdvance repacked && map geometry placed == map geometry alternatePlaced)
        (fail "repacking changed text metrics or positions")
      let alpha = BS.pack [BS.index rgba (i * 4 + 3) | i <- [0 .. width * height - 1]]
          header = BSC.pack ("P5\n" ++ show width ++ " " ++ show height ++ "\n255\n")
      unless (BS.any (> 0) alpha) (fail "atlas contains no visible pixels")
      BS.writeFile outputPath (header <> alpha)
      putStrLn ("atlas=" ++ show width ++ "x" ++ show height)
    _ -> fail "Usage: font-check FONT FACE CATALOGUE_UTF8 OUTPUT_PGM"
  where geometry (TextQuad rectangle _) = rectangle

success :: Show e => Either e a -> IO a
success = either (fail . show) pure

-- Compare the production decoder with RGBA8 fixture references.
module Main (main) where

import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.List (sort)
import GameIO.Image
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>), replaceExtension, takeExtension)

main :: IO ()
main = do
  [directory] <- getArgs
  names <- sort . filter (\name -> elem (takeExtension name) [".png", ".bmp"]) <$> listDirectory directory
  unless (not (null names)) (fail "No image fixtures.")
  forM_ names $ \name -> do
    encoded <- BS.readFile (directory </> name)
    expected <- BS.readFile (directory </> replaceExtension name "rgba")
    actual <- either (fail . ((name ++ " ") ++) . show) pure (decodeImage encoded)
    let (width, height) = imageSize actual
        firstWord i = BS.foldr (\byte value -> fromIntegral byte + 256 * value) 0 (BS.take 4 (BS.drop i expected))
    unless ((width, height) == (firstWord 0, firstWord 4) && imageRGBA actual == BS.drop 8 expected)
      (fail (name ++ " extent or pixels differ from the reference."))
    case decodeImage (BS.take (BS.length encoded - 5) encoded) of
      Left _ -> pure ()
      Right _ -> fail (name ++ " accepted truncated data.")
    putStrLn (name ++ " exact")
  putStrLn (show (length names) ++ " images")

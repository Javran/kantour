module Kantour.Sprite where

import Data.Aeson
import Kantour.Sprite.Spritesmith (FileInfo (..))
import Kantour.Subcommand
import Shower
import System.Environment
import System.Exit (die)

{-
  TODO:

  A tool for extracting spritesmith from kc assets:

  - general purpose sprite extraction
  - specialized for extracting ship tags for events

 -}

data SubCmdSprite

instance Subcommand SubCmdSprite where
  name _ = "Sprite"
  main _ = defaultMain

defaultMain :: IO ()
defaultMain =
  getArgs
    >>= \case
      ["parse", fp] -> do
        result <- eitherDecodeFileStrict' @FileInfo fp
        case result of
          Left err -> die $ "parse error: " <> err
          Right fi -> printer fi
      args -> die $ "Unknown: " <> show args

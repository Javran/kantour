module Kantour.Sprite where

import Data.Aeson
import Kantour.Sprite.Spritesmith (FileInfo (..))
import Kantour.Subcommand
import Shower
import System.Environment
import System.Exit (die)
import Text.ParserCombinators.ReadP

{-

  A tool for extracting spritesmith from kc assets for:

  - general purpose sprite extraction
  - specialized for extracting ship tags for events

  (TODO) Subcommands:

  - `fetch <src> <dst dir>`: download from <src> to <dst dir>

    + `w1:<relative url><optional query str>`: download from w01 server
    + `http...`: download from a url

    The url part (without query str) should ends with
    either `.json` or `.png` - the tool should ensure both are fetched.

  - `extract <json>`: load local JSON file, extract to the same directory
    where that file resides.

  - `tag-extract`: syntax TBD, extract ship tags in one command.

 -}

data SubCmdSprite

instance Subcommand SubCmdSprite where
  name _ = "Sprite"
  main _ = defaultMain

-- sourceP :: ReadP

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

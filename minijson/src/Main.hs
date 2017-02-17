module Main where

import Prelude hiding (takeWhile)
import qualified Data.Text.IO as T
import Data.Attoparsec.Text
import System.Environment

import MiniJson.Parser

-- instead of what Data.Aeson does,
-- we actually want to work on top of object key order preserving data structure
-- which motivates this small project.

main :: IO ()
main = do
    [srcFP] <- getArgs
    content <- T.readFile srcFP
    let parsed = parseOnly pValue content
    print parsed

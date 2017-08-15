module Kantour.MiniJson.Main where

import Prelude hiding (takeWhile)
import qualified Data.Text.IO as T
import Data.Attoparsec.Text
import System.Environment
import Text.PrettyPrint.HughesPJClass
import System.Exit

import Kantour.MiniJson.Parser
import Kantour.Subcommand

data SubCmdMiniJson

instance Subcommand SubCmdMiniJson  where
    name _ = "MiniJson"
    main _ = defaultMain

-- instead of what Data.Aeson does,
-- we actually want to work on top of object key order preserving data structure
-- which motivates this small project.

defaultMain :: IO ()
defaultMain = do
    [srcFP] <- getArgs
    content <- T.readFile srcFP
    let parsed = parseOnly (skipSpace >> pValue) content
    case parsed of
        Right json ->
            putStrLn (prettyShow json)
        Left _ -> exitFailure

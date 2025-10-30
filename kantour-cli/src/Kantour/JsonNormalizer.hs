module Kantour.JsonNormalizer
  ( SubCmdJsonNormalizer
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as M
import Kantour.Subcommand
import System.Environment
import System.Exit

{-
  Normalizes any JSON file by sorting all Object keys.

  Though key order does not matter according to JSON spec,
  it's possible to hide extra bits of info by re-arranging
  those keys. This tool is an attempt at destroying this kind
  of information / nondeterminism from JSON data.

 -}

data SubCmdJsonNormalizer

instance Subcommand SubCmdJsonNormalizer where
  name _ = "JsonNormalizer"
  main _ = defaultMain

normalize :: Value -> Value
normalize = \case
  Object m ->
    let
      xs = KM.toAscList m
      m' = M.map normalize $ M.fromDistinctAscList xs
     in
      Object (KM.fromMap m')
  Array xs -> Array (fmap normalize xs)
  x@(String _) -> x
  x@(Number _) -> x
  x@(Bool _) -> x
  Null -> Null

defaultMain :: IO ()
defaultMain =
  getArgs
    >>= \case
      [inFp, outFp] -> do
        r <- eitherDecodeFileStrict' @Value inFp
        case r of
          Left err -> die $ "parse error: " <> err
          Right v ->
            encodeFile outFp (normalize v)
      _ -> die "<prog> <input> <output>"

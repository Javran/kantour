{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Kantour.MapTwol.Main where

{-
  TODO: status: under construction.
 -}

import Control.Monad
import Data.Aeson
import qualified Data.Vector as Vec
import Kantour.KcData.Map.Image as D
import Kantour.Subcommand
import System.Environment

data SubCmdMapTwol

instance Subcommand SubCmdMapTwol where
  name _ = "MapTwol"
  main _ = defaultMain

defaultMain :: IO ()
defaultMain =
  getArgs >>= \case
    ["dev-combine", srcFileList, dstFile] -> do
      {-
        Accepts a list of JSON file paths, read them and combine
        it into an Array.

        The purpose of this is to see if it's possible to infer types from
        multiple samples using https://github.com/jvilk/MakeTypes.
       -}
      fps <- lines <$> readFile srcFileList
      vals <- forM fps $ \fp ->
        eitherDecodeFileStrict @Value fp >>= \case
          Left msg -> error $ "failed to parse " <> fp <> ": " <> msg
          Right v -> pure v
      let resultObj = Array $ Vec.fromList vals
      encodeFile dstFile resultObj
    ["parse-all", srcFileList] -> do
      fps <- lines <$> readFile srcFileList
      (vals :: [D.Image]) <- forM fps $ \fp ->
        eitherDecodeFileStrict @D.Image fp >>= \case
          Left msg -> error $ "failed to parse " <> fp <> ": " <> msg
          Right v -> pure v
      -- let Just bgs = (traverse . traverse) (Just . bg) $ fmap (: []) vals
      putStrLn $ "all " <> show (length fps) <> " files parsed."
      -- mapM_ print bgs
    _ -> do
      putStrLn "<file list> <target file>"

{-# LANGUAGE FlexibleContexts #-}
module Kantour.Coded.Main
  ( defaultMain
  ) where

import System.Environment

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Control.Monad.State
import Data.Monoid

data BinData = BD
  { _bdFirst :: ByteString
  , _bdShuffled :: Maybe [ByteString] -- should always be of length 8
  , _bdLeftover :: ByteString
  }

{-

replicated https://github.com/yukixz/kctools/blob/master/Core-decode.py

for fun.

-}

blockOrder :: [Int]
blockOrder = [0, 7, 2, 5, 4, 3, 6, 1]

unshuffle :: BinData -> BinData
unshuffle bd@BD { _bdShuffled = bdS } = bd { _bdShuffled = bdS' }
  where
    bdS' = (\xs -> map (xs !!) blockOrder) <$> bdS

toBinData :: ByteString -> BinData
toBinData = evalState createBinData
  where
    createBinData = do
        bdFst <- loadWords 128
        eof <- testEof
        if eof
          then pure (BD bdFst Nothing BS.empty)
          else do
            -- remaining length
            l <- gets BS.length
            let blockSz = l `div` 8
            blocks <- replicateM 8 (loadWords blockSz)
            leftover <- state (\s -> (s,BS.empty))
            pure (BD bdFst (Just blocks) leftover)
      where
        loadWords x = state (BS.splitAt x)
        testEof = gets ((== 0) . BS.length)

fromBinData :: BinData -> ByteString
fromBinData (BD bdFst bdS bdL) = bdFst <> maybe mempty mconcat bdS <> bdL

{-| default entry point for @coded@
-}
defaultMain :: IO ()
defaultMain = do
    as <- getArgs
    case as of
        (srcFP:dstFP:_) -> do
             raw <- BS.readFile srcFP
             let unshuffledBD = unshuffle . toBinData $ raw
                 unshuffled = fromBinData unshuffledBD
             BS.writeFile dstFP unshuffled
             putStrLn $ "File size: " ++ show (BS.length raw)
             putStrLn $ "Leftover size: " ++ show (BS.length . _bdLeftover $ unshuffledBD)
        _ -> putStrLn "<prog> <src file> <dst file>"

{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.Environment

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Control.Monad.State
import Data.Monoid
import Data.Maybe

data BinData = BD
  { bdFirst :: ByteString
  , bdShuffled :: Maybe [ByteString] -- should always be of length 8
  , bdLeftover :: ByteString
  }

{-

replicated https://github.com/yukixz/kctools/blob/master/Core-decode.py

for fun.

-}

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

main :: IO ()
main = do
    (srcFP:dstFP:_) <- getArgs
    pure ()

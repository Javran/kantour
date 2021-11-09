module Kantour.Core.DataFiles
  ( loadDataFile
  , loadDataFileStrict
  , toPlainData
  )
where

import qualified Codec.Compression.Lzma as Lzma
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Paths_kantour_core

{-
  Either decompress .xz format or keep it as it is depending on file extension.
 -}
toPlainData :: FilePath -> BSL.ByteString -> BSL.ByteString
toPlainData fp =
  if ".xz" `isSuffixOf` fp
    then Lzma.decompress
    else id

loadDataFile :: FilePath -> IO BSL.ByteString
loadDataFile fpPre = do
  fp <- getDataFileName fpPre
  toPlainData fp <$> BSL.readFile fp

loadDataFileStrict :: FilePath -> IO BS.ByteString
loadDataFileStrict = fmap BSL.toStrict . loadDataFile

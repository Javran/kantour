module Kantour.MapToolFast.Main where

import qualified Data.ByteString as BS
import System.Environment

import Kantour.Subcommand
import qualified Text.XML.Hexml as Xml

data SubCmdMapToolFast

instance Subcommand SubCmdMapToolFast where
  name _ = "MapToolFast"
  main _ = defaultMain

defaultMain :: IO ()
defaultMain = do
  [fName] <- getArgs
  content <- BS.readFile fName
  let Right n = Xml.parse content
  mapM_ print (Xml.name <$> (Xml.children $ (Xml.children n) !! 1))

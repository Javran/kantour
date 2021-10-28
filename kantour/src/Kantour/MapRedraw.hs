{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Kantour.MapRedraw where

import System.Environment
import System.Exit
import Kantour.Subcommand
import Kantour.MapTool.Types
import Kantour.MapTool.Draw
import Data.String
import qualified Data.Set as S
import Linear
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M

import Data.Aeson
import Data.Aeson.Encode.Pretty

data SubCmdMapRedraw

instance Subcommand SubCmdMapRedraw where
    name _ = "MapRedraw"
    main _ = defaultMain

newtype PoiFcdMapInfo = PoiMI MapInfo

instance ToJSON PoiFcdMapInfo where
    toJSON (PoiMI mi) = object ["route" .= route , "spots" .= spots]
      where
        ls = _miLines mi
        starts = _miStarts mi
        nodes = _miNodeNames mi
        route :: Value
        route = object $ ("0" .= [Null, "1"]) : fmap tr ls
          where
            tr (MyLine nm nStart nEnd) = fromString (cut nm) .= fmap nodeToName [nStart, nEnd]
            nodeToName :: V2 Int -> Value
            nodeToName coord
                | Just nName <- M.lookup coord nodes = fromString nName
                | otherwise = Null
            cut :: String -> String
            cut xs
                | ("line", ys) <- splitAt 4 xs = ys
                | otherwise = error "prefix 'line' expected"

        spots = object $ fmap tr (M.toList nodes)
          where
            tr (coord@(V2 x y), n) = fromString n .= [toJSON x, toJSON y, mayStart]
              where
                mayStart = if coord `S.member` starts then "start" else ""

{-

allow reading map info and redraw the image without having
to repeat the process of parsing.

this helps the correction of node names found in mapInfo

-}
-- -w 800 -h 400 -o test.png
defaultMain :: IO ()
defaultMain = do
    args <- getArgs
    let helpAndExit = do
            putStrLn "MapRedraw <mapInfo> <output image> <output poifcd>"
            exitSuccess
    case args of
        [] -> helpAndExit
        [miFilePath, outFilePath, outFcdPath] -> do
            raw <- readFile miFilePath
            case reads raw of
                [(mapInfo :: MapInfo, _)] -> do
                      withArgs (words "-w 800 -h 400 -o" ++ [outFilePath]) $ draw mapInfo
                      let encoded = encodePretty' config (PoiMI mapInfo)
                          config = defConfig {confCompare = compare}
                      LBS.writeFile outFcdPath encoded
                _ -> putStrLn "failed to parse the content"
        _ -> helpAndExit

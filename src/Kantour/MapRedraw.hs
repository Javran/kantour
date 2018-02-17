{-# LANGUAGE ScopedTypeVariables #-}
module Kantour.MapRedraw where

import System.Environment
import System.Exit
import Kantour.Subcommand
import Kantour.MapTool.Types
import Kantour.MapTool.Draw

data SubCmdMapRedraw

instance Subcommand SubCmdMapRedraw where
    name _ = "MapRedraw"
    main _ = defaultMain

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
            putStrLn "MapRedraw <mapInfo> <output image>"
            exitSuccess
    case args of
        [] -> helpAndExit
        [miFilePath, outFilePath] -> do
            raw <- readFile miFilePath
            case reads raw of
                [(mapInfo :: MapInfo, _)] ->
                      withArgs (words "-w 800 -h 400 -o" ++ [outFilePath]) $ draw mapInfo
                _ -> putStrLn "failed to parse the content"
        _ -> helpAndExit

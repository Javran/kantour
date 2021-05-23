{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fwarn-partial-type-signatures #-}

module Kantour.MapTool.Main where

import Control.Monad
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Yaml as Yaml
import Kantour.MapTool.Draw
import Kantour.MapTool.MapConfig
import Kantour.MapTool.Types
import Kantour.MapTool.Xml
import Kantour.Subcommand
import Linear
import Linear.Affine
import System.Environment
import System.Exit
import System.FilePath
import Text.JSON
import Text.Printf

data SubCmdMapTool

instance Subcommand SubCmdMapTool where
  name _ = "MapTool"
  main _ = defaultMain

{-

implementation of:

https://github.com/yukixz/kcmap/blob/master/kcmap.es

in Haskell

and perhaps more.

related links:

an explanation can be found at:

http://blog.dazzyd.org/blog/how-to-draw-a-kancolle-map/

-}

{-

terms: (pick the most precise term on conflict)

- "main" for things that can be tracked back to sprite with name "map"
- "extra" for things that can be tracked back to a sprite with name prefixed "extra"
- "hidden" for things that come from the separated encoded binary file.

-}

{-

TODO:

- allow gradual correction of node names

    - add ".nodes.json" to main map file to produce the node name mapping.
      e.g. if main map file is "37_01.xml", mapping file will be called "37_01.xml.nodes.json"

- none-overlapping of edges...

-}

{-

TODO: argument parsing is getting complicated, let's try using a structured config file
to tell the program what to do.

see "example.yaml"

-}

loadFromConfig :: FilePath -> IO ()
loadFromConfig fp = do
  result <- Yaml.decodeFileEither fp :: IO (Either Yaml.ParseException MapConfig)
  case result of
    Left err -> putStrLn $ "error while parsing config '" ++ fp ++ "': " ++ show err
    Right mapConf ->
      print mapConf

-- TODO: tmp entry point for debugging
-- defaultMain :: IO ()
-- defaultMain = loadFromConfig "example.yaml"

-- stack build && stack exec -- maptool 38-3.xml sm38.xml 383 -- -o 38-3.svg -w 2000 && ristretto 38-3.svg
defaultMain :: IO ()
defaultMain = do
  mArgs <- sepArgs <$> getArgs
  case mArgs of
    Nothing -> do
      putStrLn "invalid arguments"
      putStrLn "usage: maptool <main xml> [hidden xml <search tag>] [-- diagrams args]"
      putStrLn "the argument list passing to diagrams, if exists, has to be non empty"
      exitFailure
    Just ((srcFP, mHiddenFPInfo), mDiagramArgs) -> do
      -- pretty printing arguments
      putStrLn $ "main xml: " ++ srcFP
      putStrLn $ "hidden xml: " ++ maybe "<N/A>" fst mHiddenFPInfo
      putStrLn $ "args to diagrams: " ++ maybe "<N/A>" unwords mDiagramArgs
      let miFP = srcFP <.> "mi"
      (mainRoutes, mainBeginNodes) <- safeParseXmlDoc extractFromMain srcFP
      case mHiddenFPInfo of
        Just (hiddenFP, _) -> do
          parsed <- parseXmlDoc findHiddenSpriteRoots hiddenFP
          case parsed of
            Left errMsg ->
              putStrLn $ "Parse error: " ++ errMsg
            Right vss ->
              let vs = concat vss
                  ppr (sId, sName) = printf "Id: %s\tName: %s\n" sId sName
               in mapM_ ppr vs
        Nothing -> pure ()
      (hiddenRoutes, hiddenBeginNodes) <-
        case mHiddenFPInfo of
          Just (hiddenFP, searchTag) ->
            safeParseXmlDoc (extractFromHidden searchTag) hiddenFP
          Nothing -> pure ([], [])
      putStrLn "====="
      -- the coordinates look like large numbers because SWF uses twip as basic unit
      -- (most of the time) divide them by 20 to get pixels
      let beginNodes = mainBeginNodes ++ hiddenBeginNodes
          adjustedRoutes = adjustLines beginNodes (mainRoutes ++ hiddenRoutes)
          pointMap = mkPointMap beginNodes adjustedRoutes
          mapInfo = MapInfo adjustedRoutes (S.fromList beginNodes) pointMap
      case mDiagramArgs of
        Nothing -> pure ()
        Just diagramArgs -> withArgs diagramArgs $ draw mapInfo
      putStrLn "=== JSON encoding ==="
      putStrLn (encodeStrict (linesToJSValue adjustedRoutes pointMap))
      writeFile miFP (show mapInfo)
      putStrLn $ "Written map info to: " ++ miFP

-- TODO (new) search tag: "385" means "scene.sally.mc.MCCellSP385", etc.
-- separate argument list into maptool arguments and those meant for diagrams:
-- arg list: <main xml> [hidden xml <search tag>] [-- <diagram args>]
-- where <main xml> is the map xml file, [hidden xml] is an optional part.
-- additionally, if "--" exists and <diagram args> is not empty, diagram will be called
-- to draw a picture.
sepArgs :: [String] -> Maybe ((String, Maybe (String, String)), Maybe [String])
sepArgs as = do
  let (ls, rs') = break (== "--") as
  lVal <- case ls of
    [] -> Nothing
    mainXmlFP : ls' -> case ls' of
      [] -> pure (mainXmlFP, Nothing)
      [extraXmlFP, searchTag] -> pure (mainXmlFP, Just (extraXmlFP, searchTag))
      _ -> Nothing
  let rVal = case rs' of
        [] -> Nothing
        -- the "_" part as to be "--" as it's the result from "break"
        _ : xs -> guard (not (null xs)) >> pure xs
  pure (lVal, rVal)

{-
begin point of each edge is estimated from end point and the shape info of the line
so we need to adjust begin points for each line, this is done by picking the closest
"confirmed point" from the estimated begin point.

"confirmed point" includes begin points of a map, and end point of all edges.
-}
adjustLines :: [V2 Int] -> [MyLine] -> [MyLine]
adjustLines startPts ls = adjustLine <$> ls
  where
    confirmedPoints = startPts ++ (_lEnd <$> ls)
    adjustLine :: MyLine -> MyLine
    adjustLine l@(MyLine _ lStartPt _) = l {_lStart = adjustedStartPt}
      where
        adjustedStartPt = minimumBy (compare `on` qdA lStartPt) confirmedPoints

{-
guess names for each node:

- begin nodes are named "<n>" where n is a number.
  However just keep that in mind that
  in KC3Kai edges.json file, there's no distinction between begin nodes and all are called just "Start".

- for all the other nodes, the name of a node depends on the name of edges pointing to it.
  for an edge with name "line1", this will be "A", and "B" for "line2", "C" for "line3" etc.
  if there are multiple edges pointing to one node, one with the least number wins.

- note that these naming rules are not always working. so one needs to take a closer look on generated data.

-}
mkPointMap :: [V2 Int] -> [MyLine] -> M.Map (V2 Int) String
mkPointMap beginNodes xs = M.union beginNodeNames endNodeNames
  where
    beginNodeNames = M.fromList (zip beginNodes (formatName <$> [1 :: Int ..]))
      where
        formatName x = "<" ++ show x ++ ">"

    -- collect all possible names and pick the minimal one
    endNodeNames =
      M.map
        getMin
        (M.fromListWith
           (++)
           (map convert xs))

    getMin = minimumBy (\x y -> compare (length x) (length y) <> compare x y)
    lineToInt l = read (simpleLName l)
    nodeNameFromInt v
      | v -1 < length ns = ns !! (v -1)
      | otherwise = show v
      where
        ns = map (: []) ['A' .. 'Z']

    convert l = (_lEnd l, [nodeNameFromInt . lineToInt $ l])

linesToJSValue :: [MyLine] -> M.Map (V2 Int) String -> JSValue
linesToJSValue xs nnames = JSObject (toJSObject (convert <$> ys))
  where
    ys = sortBy (compare `on` (\l -> read (simpleLName l) :: Int)) xs
    getNm v = makeStart (fromMaybe "Unknown" (M.lookup v nnames))
      where
        makeStart ('<' : _) = "Start"
        makeStart v' = v'
    convert :: MyLine -> (String, JSValue)
    convert l = (simpleLName l, JSArray (f <$> [getNm (_lStart l), getNm (_lEnd l)]))
      where
        f = JSString . toJSString

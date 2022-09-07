module Kantour.DecMapUrl (
  defaultMain,
  SubCmdDecMapUrl,
) where

import Control.Monad
import Data.Bits
import Data.Char
import Data.Function
import Data.List
import Kantour.Subcommand
import System.Environment
import System.Exit
import System.Random
import Text.Printf

data SubCmdDecMapUrl

instance Subcommand SubCmdDecMapUrl where
  name _ = "DecMapUrl"
  main _ = defaultMain

serverIps :: [String]
serverIps =
  [ "203.104.209.71"
  , "203.104.209.87"
  , "125.6.184.16"
  , "125.6.187.205"
  , "125.6.187.229"
  , "125.6.187.253"
  , "125.6.188.25"
  , "203.104.248.135"
  , "125.6.189.7"
  , "125.6.189.39"
  , "125.6.189.71"
  , "125.6.189.103"
  , "125.6.189.135"
  , "125.6.189.167"
  , "125.6.189.215"
  , "125.6.189.247"
  , "203.104.209.23"
  , "203.104.209.39"
  , "203.104.209.55"
  , "203.104.209.102"
  ]

genServerIp :: IO String
genServerIp = do
  ind <- randomRIO (0, length serverIps - 1)
  pure (serverIps !! ind)

rndToHexs :: [Int] -> [Int]
rndToHexs = concatMap conv
  where
    conv :: Int -> [Int]
    conv x
      | x > 10000 =
        let upper = ((x .&. 0xFF00) `shiftR` 8)
            lower = x
         in (.&. 0xFF) <$> [upper, lower]
      | otherwise =
        let asHex v = let (u, l) = v `quotRem` 10 in u * 16 + l
            (upper', lower') = x `quotRem` 100
         in map asHex [upper', lower']

consume :: [Int] -> Maybe ({- result -} (String, String {- leftover -}), [Int])
consume [] = Nothing
consume (x : xs)
  | x < 90 =
    let (ys, left) = span (>= 90) xs
        ds = ['0' .. '9']
        (u, l) = x `quotRem` 16
     in Just ((map (ds !!) [u, l], map chr ys), left)
  | otherwise = Nothing

-- RND: <Core>.scripts/common/resources/MapResourceLoader
defaultMain :: IO ()
defaultMain = do
  args <- getArgs
  when (null args) $ do
    putStrLn "decmapurl <RND file> [area]"
    exitFailure
  let (fp : mArea) = args
  [(rnd :: [Int], _)] <- reads <$> readFile fp
  let hexs = rndToHexs rnd
      mapData =
        map (\xs -> (fst (head xs), snd <$> xs))
          . groupBy ((==) `on` fst)
          . filter ((/= "51") . fst)
          $ unfoldr consume hexs
  putStrLn $ "Available areas: " ++ unwords (fst <$> mapData)
  case mArea of
    (area : _) -> do
      putStrLn $ "Selected area: " ++ area
      let maps =
            snd
              . head
              . filter ((== area) . fst)
              $ mapData
          mapPairs = zip [1 ..] maps
          process (sub :: Int, fileName) = do
            serverIp <- genServerIp
            let url =
                  concat
                    [ "http://"
                    , serverIp
                    , "/kcs/resources/swf/map/"
                    , fileName ++ ".swf"
                    ]
                mapStr = area ++ "-" ++ show sub
            printf "aria2c -o '%s.swf' '%s'\n" mapStr url
      mapM_ process mapPairs
    _ -> pure ()

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}


module Kantour.FetchMasterData where
import Kantour.Subcommand
data SubCmdFetchMasterData

{-
  TODO: the need of getting and parsing api_start2
  comes up so often that it might worth making into enviroment variables.
 -}

instance Subcommand SubCmdFetchMasterData where
  name _ = "FetchMasterData"
  main _ = defaultMain

defaultMain :: IO ()
defaultMain =
  getArgs >>= \case
    [fileOrUrlSrc] -> do
      mgr <- newManager tlsManagerSettings
      rawJson <- loadFromSource mgr fileOrUrlSrc
      CollectExtra {ceValue = r, ceExtra} <-
        case Aeson.eitherDecode @(CollectExtra MasterRoot) rawJson of
          Left msg -> die ("parse error: " <> msg)
          Right r -> pure r
      putStrLn "Ship sample:"
      printer (head $ mstShip r)
      unless (null ceExtra) $ do
        putStrLn "Following fields are not yet accounted for:"
        forM_ (sortOn fst ceExtra) $ \(k, v) -> do
          putStrLn $ "- " <> T.unpack k
          let valToStr = T.unpack . decodeUtf8 . BSL.toStrict . encode
          case v of
            Array xs -> do
              putStrLn "Value is an array, showing first 2 elements:"
              mapM_ (putStrLn . valToStr) (V.take 2 xs)
            _ -> do
              putStrLn "Truncated sample: "
              putStrLn (take 2000 $ valToStr v)
    _ -> die "<subcmd> [http]<source>"

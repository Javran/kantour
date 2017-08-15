module Kantour.Subcommand where

import Data.Proxy

class Subcommand s where
  name :: Proxy s -> String
  main :: Proxy s -> IO ()

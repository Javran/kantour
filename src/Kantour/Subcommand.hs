module Kantour.Subcommand where

class Subcommand s where
  name :: s -> String
  main :: s -> IO ()

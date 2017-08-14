{-# LANGUAGE NamedFieldPuns #-}
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo

main :: IO ()
main = defaultMainWithHooks (simpleUserHooks {postBuild})
  where
    -- TODO
    postBuild _ _ _ LocalBuildInfo {buildDir} = pure ()

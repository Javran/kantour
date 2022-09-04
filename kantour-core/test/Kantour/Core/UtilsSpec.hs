module Kantour.Core.UtilsSpec where

import Control.Monad
import Kantour.Core.Utils
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "alterAL" $ do
    let ex1 = [(1 :: Int, 10 :: Int)]
        ex2 = (2, 20) : (3, 30) : ex1
    specify "insertion behaviors" $ do
      insertAL 1 10 [] `shouldBe` ex1
      insertAL 1 10 [(1, 20)] `shouldBe` ex1
      insertAL 5 50 ex2 `shouldBe` ex2 ++ [(5, 50)]
      insertAL 3 0 ex2 `shouldBe` [(2, 20), (3, 0), (1, 10)]
    specify "deletion behaviors" $ do
      deleteAL 1 [] `shouldBe` ([] `asTypeOf` ex1)
      deleteAL 1 ex1 `shouldBe` []
      deleteAL 3 ex2 `shouldBe` [(2, 20), (1, 10)]
    specify "toggle one element" $ do
      let toggle Nothing = Just 1
          toggle (Just _) = Nothing
      alterAL toggle 6 ex1 `shouldBe` [(1, 10), (6, 1)]
      alterAL toggle 3 ex2 `shouldBe` [(2, 20), (1, 10)]
  describe "stripR" $ do
    let stripR' = reverse . stripL . reverse
    prop "general correctness" $
      \xs -> stripR xs === stripR' xs
    prop "dense whitespace" $ do
      let charGen = elements (['a' .. 'z'] ++ ['A' .. 'Z'])
          spaceGen = elements " \t\v\n\f\r"
          cGen = do
            which <- choose (0, 9)
            if which < (3 :: Int)
              then charGen -- 30% chance of generating a-z A-Z
              else spaceGen -- 70% chance of generating whitespaces
      l <- choose (0, 20)
      xs <- replicateM l cGen
      pure $ counterexample xs (stripR xs === stripR' xs)

module Kantour.GameResource.MagicSpec where

import Test.Hspec
import Kantour.GameResource.Magic

spec :: Spec
spec =
  describe "magicCode" $
    specify "examples" $ do
      magicCode 184 "ship_banner" `shouldBe` 4357
      magicCode 184 "ship_card" `shouldBe` 5681

      magicCode 185 "ship_banner" `shouldBe` 5755
      magicCode 185 "ship_card" `shouldBe` 2050

      magicCode 318 "ship_banner" `shouldBe` 6692
      magicCode 318 "ship_card" `shouldBe` 6320

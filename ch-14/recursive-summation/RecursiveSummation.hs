module RecursiveSummation where

import Test.Hspec

mul :: (Integral a) => a -> a -> a
mul _ 0 = 0
mul n times = n + (mul n (times-1))

main :: IO ()
main = hspec $ do
  describe "Multiplication by recursive summation" $ do
    it "mul 1 0 is 0" $ do
      mul 1 0 `shouldBe` 0
    it "mul 1 1 is 1" $ do
      mul 1 1 `shouldBe` 1
    it "mul 5 3 is 15" $ do
      mul 5 3 `shouldBe` 15

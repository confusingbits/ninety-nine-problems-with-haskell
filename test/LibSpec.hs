module LibSpec (spec) where

import Problems
import Test.Hspec

spec :: Spec
spec = do
  describe "problem1" $ do
    context "When given [1,2,3,4]" $ do
      it "should return 4" $ do
        problem1 [1,2,3,4] `shouldBe` 4
    context "When given ['x','y','z']" $ do
      it "should return 'z'" $ do
        problem1 ['x','y','z'] `shouldBe` 'z'
  describe "problem2" $ do
    context "When given [1,2,3,4]" $ do
      it "should return 3" $ do
        problem2 [1,2,3,4] `shouldBe` 3
    context "When given ['x','y','z']" $ do
      it "should return 'y'" $ do
        problem2 ['x','y','z'] `shouldBe` 'y'
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
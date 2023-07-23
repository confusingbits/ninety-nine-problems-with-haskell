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
  describe "problem3" $ do
    context "When given [1,2,3] and 2" $ do
      it "should return 2" $ do
        problem3 [1,2,3] 2 `shouldBe` 2
    context "When given 'haskell'" $ do
      it "should return 'e'" $ do
        problem3 "haskell" 5 `shouldBe` 'e'
  describe "problem4" $ do
    context "When given [123, 456, 789]" $ do
      it "should return 3" $ do
        problem4 [123, 456, 789] `shouldBe` 3
    context "When given 'Hello, world!'" $ do
      it "should return 13" $ do
        problem4 "Hello, world!" `shouldBe` 13
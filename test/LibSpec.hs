module LibSpec (spec) where

import Problems
import Test.Hspec

spec :: Spec
spec = do
  describe "problem1" $ do
    context "When given [1,2,3,4]" $ do
      it "should return 4" $ do
        problem1 [1, 2, 3, 4] `shouldBe` 4
    context "When given ['x','y','z']" $ do
      it "should return 'z'" $ do
        problem1 ['x', 'y', 'z'] `shouldBe` 'z'
  describe "problem2" $ do
    context "When given [1,2,3,4]" $ do
      it "should return 3" $ do
        problem2 [1, 2, 3, 4] `shouldBe` 3
    context "When given ['x','y','z']" $ do
      it "should return 'y'" $ do
        problem2 ['x', 'y', 'z'] `shouldBe` 'y'
  describe "problem3" $ do
    context "When given [1,2,3] and 2" $ do
      it "should return 2" $ do
        problem3 [1, 2, 3] 2 `shouldBe` 2
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
  describe "problem5" $ do
    context "When given 'A man, a plan, a canal, panama!'" $ do
      it "should return the reverse string" $ do
        problem5 "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"
      it "should return [4,3,2,1]" $ do
        problem5 [1, 2, 3, 4] `shouldBe` [4, 3, 2, 1]
  describe "problem6" $ do
    context "When given [1,2,3]" $ do
      it "should return False" $ do
        problem6 [1, 2, 3] `shouldBe` False
    context "When given 'madamimadam'" $ do
      it "should return True" $ do
        problem6 "madamimadam" `shouldBe` True
    context "When given [1,2,4,8,16,8,4,2,1]" $ do
      it "should return True" $ do
        problem6 [1, 2, 4, 8, 16, 8, 4, 2, 1] `shouldBe` True
  describe "problem7" $ do
    context "When given (Elem 5)" $ do
      it "should return [5]" $ do
        problem7 (Elem 5) `shouldBe` [5]
    context "When given (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])" $ do
      it "should return [1,2,3,4,5]" $ do
        problem7 (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1,2,3,4,5]
    context "When given (List [])" $ do
      it "should return []" $ do
        problem7 (List []) `shouldBe` ([] :: [Int]) -- idk why this needs to be specified
  describe "problem8" $ do
    context "When given 'aaaabccaadeeee'" $ do
      it "should return 'abcade'" $ do
        problem8 "aaaabccaadeeee" `shouldBe` "abcade" 
module LibSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "foo" $ do
    it "bar" $ do
      pendingWith "this is test automaticly"
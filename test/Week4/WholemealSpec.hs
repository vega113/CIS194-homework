module Week4.WholemealSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Week4.Wholemeal

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "fun1'" $ do
    it "computes the same value as fun1 1" $ do fun1 [] `shouldBe` 1
    it "computes the same value as fun1 2" $ do fun1 [2] `shouldBe` 0
    it "computes the same value as fun1 3" $ do fun1 [3] `shouldBe` 1
    it "computes the same value as fun1 4" $ do fun1 [3, 4] `shouldBe` 2
    it "fun1 and fun1' should produce equal outputs" $ property $ \l -> fun1 l === fun1' l
--    it "fun2 and fun2' should produce equal outputs" $ property $ \x -> fun2 x === fun2' x

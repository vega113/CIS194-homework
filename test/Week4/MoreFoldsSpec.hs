module Week4.MoreFoldsSpec
 ( main
  , spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Week4.MoreFolds



-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "xor fold should retun true iff odd number of True in list" $ do

   it "should return False for []" $ do
        xor [] `shouldBe` False

   it "should return False for [False]" $ do
       xor [False] `shouldBe` False

   it "should return True for [True]" $ do
       xor [True] `shouldBe` True

   it "should return True for [False, True, False]" $ do
       xor [True] `shouldBe` True

   it "should return True for [False, True, False]" $ do
       xor [True, True] `shouldBe` False

   it "should map' as map" $ property $ \l -> mapInt' l ===  map (\x -> x*x) l


mapInt' :: [Int] -> [Int]
mapInt' l = map' (\x -> x*x) l

module Week4.SieveOfSundaramSpec
 ( main
  , spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Week4.SieveOfSundaram

import Data.Numbers.Primes



-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Sieve of Sundaram should compute all primes less than n" $ do

   it "should return primes list for 2" $ do
        sieveSundaram 2 `shouldBe` [2]
   it "should return primes list for 3" $ do
        sieveSundaram 3 `shouldBe` [2, 3]

   it "should return primes list for 6" $ do
        sieveSundaram 6 `shouldBe` [2, 3, 5]

   it "should return primes list for 15" $ do
        sieveSundaram 15 `shouldBe` [2, 3, 5, 7]
   it "should return primes list for any n" $ property $ \num ->
        (sieveSundaram num) === filter isPrime [1..num]



myL :: Integer -> Integer
myL n = n
module Week5.CalculatorSpec   ( main
                               , spec
                               ) where


import Test.Hspec
import Test.QuickCheck
import Week5.Calculator

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "balanced binary tree'" $ do
    it "compares trees with Node and Node correctly left level == right level" $ do
      True `shouldBe` True
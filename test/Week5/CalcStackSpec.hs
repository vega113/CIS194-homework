module Week5.CalcStackSpec   ( main
                               , spec
                               ) where


import Test.Hspec
import Test.QuickCheck
import Week5.CalcStack
import Week5.Parser
import Week5.StackVM
import Week5.Expr

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "CalcStack" $ do
    it "evaluates program expression" $
     ((mul (add (lit 2) (lit 3)) (lit 4))::Program) `shouldBe` [Mul,Add,PushI 2,PushI 3,PushI 4]
    it "parses string expression" $
     compile "4*2+3" `shouldBe` Just ((mul (lit 4) (add (lit 2) (lit 3)))::Program)
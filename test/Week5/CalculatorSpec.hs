module Week5.CalculatorSpec   ( main
                               , spec
                               ) where


import Test.Hspec
import Test.QuickCheck
import Week5.Calculator
import Week5.ExprT
import Week5.Parser
import Week5.Expr

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Calc" $ do
    it "evaluates expression to 20" $
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
    it "evaluates string expression to 20" $
     evalStr "10*2" `shouldBe` Just 20
    it "converts Expr to ExprT" $
     (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) `shouldBe` (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
    it "evaluates reified expression" $
     (eval $ reify $ mul (add (lit 2) (lit 3)) (lit 4)) `shouldBe` 20
    it "evaluates integer expression" $
     ((mul (add (lit 2) (lit 3)) (lit 4))::Integer) `shouldBe` 20
    it "evaluates boolean expression" $
     ((mul (add (lit 2) (lit 3)) (lit 4))::Bool) `shouldBe` True
    it "evaluates minmax expression" $
     ((mul (add (lit 2) (lit 3)) (lit 4))::MinMax) `shouldBe` MinMax 3
    it "evaluates mod7 expression" $
     ((mul (add (lit 2) (lit 3)) (lit 4))::Mod7) `shouldBe` Mod7 6
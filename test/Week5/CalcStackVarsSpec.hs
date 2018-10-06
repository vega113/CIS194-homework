module Week5.CalcStackVarsSpec   ( main
                               , spec
                               ) where


import Test.Hspec
import Test.QuickCheck
import Week5.CalcStackVars
import Week5.Parser
import Week5.Expr
import Week5.VarExprT
import qualified Data.Map as M

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "CalcStackVars" $ do
    it "evaluates program expression" $
      (mul (add (lit 2) (lit 3)) (lit 4) :: VarExprT) `shouldBe` (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
    it "evaluates program expression with vars" $
      (add (lit 3) (var "x") :: VarExprT) `shouldBe` (Add (Lit 3) (Var "x"))
    it "evaluates program expression with bound variables just lit" $
      (withVars [("x", 6)] $ add (lit 3) (var "x")) `shouldBe` Just 9
    it "evaluates program expression with binded variables - complex" $
      (withVars [("x", 6), ("y", 3)]
       $ mul (var "x") (add (var "y") (var "x"))) `shouldBe` Just 54

withVars :: [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
module Week5.Calculator where

import Week5.ExprT
import Week5.Parser
import Week5.Expr
--import Week5.StackVM

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit i = i
  add = (+)
  mul = (*)

instance Expr Bool where
  lit i
    | i <= 0 = False
    | otherwise = True
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit i = MinMax i
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)


newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit i = Mod7 $ i `mod` 7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

reify :: ExprT -> ExprT
reify = id


eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add expr1 expr2) = eval expr1 + eval expr2
eval (Mul expr1 expr2) = eval expr1 * eval expr2

evalStr :: String -> Maybe Integer
evalStr str = fmap eval $ parseExp Lit Add Mul str


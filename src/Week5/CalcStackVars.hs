{-# LANGUAGE TypeSynonymInstances #-} {-# LANGUAGE FlexibleInstances #-}

module Week5.CalcStackVars where

import qualified Data.Map       as M
import           Week5.Expr
import           Week5.VarExprT

class HasVars a where
  var :: String -> a

instance HasVars VarExprT where
  var = Var

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit i _ = Just i
  add m1 m2 = evalBoundVars m1 m2 (+)
  mul m1 m2 = evalBoundVars m1 m2 (*)

evalBoundVars :: (M.Map String Integer -> Maybe Integer) ->
  (M.Map String Integer -> Maybe Integer) -> (Integer -> Integer -> Integer) ->
  (M.Map String Integer -> Maybe Integer)
evalBoundVars m1 m2 myFunc map =
  let m1Eval = m1 map
      m2Eval = m2 map
   in do m1Val <- m1Eval
         myFunc m1Val <$> m2Eval
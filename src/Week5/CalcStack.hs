{-# LANGUAGE TypeSynonymInstances #-} {-# LANGUAGE FlexibleInstances #-}

module Week5.CalcStack where

import           Week5.Expr
import           Week5.Parser
import           Week5.StackVM

instance Expr Program where
  lit i = [PushI i]
  add xs1 xs2 =  [Add] ++ xs1 ++ xs2
  mul xs1 xs2  = [Mul] ++ xs1 ++ xs2

compile :: String -> Maybe Program
compile = parseExp lit add mul

class HasVars a where
  var :: String -> a


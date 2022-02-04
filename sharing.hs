{-# LANGUAGE DeriveFunctor #-}

module Sharing where

import Expressions
import Abstract

-- adding Let
-- first add it as a meta-operation on Expr v

letExpr :: Expr v -> Expr (Maybe v) -> Expr v
letExpr e1 e2 = eval env e2 where
  env Nothing  = e1
  env (Just v) = Var v

-- fuse let and eval: letEval env e1 e2 = eval env (letExpr e1 e2)

letEval :: Semiring d => (v -> d) -> Expr v -> Expr (Maybe v) -> d
letEval env e1 e2 = eval gen e2 where
  gen Nothing  = eval env e1
  gen (Just v) = env v

-- extend the syntax of expressions with Let

data SExpr v
  =  SVar v
  |  SZero
  |  SOne
  |  SPlus  (SExpr v) (SExpr v)
  |  STimes (SExpr v) (SExpr v)
  |  SLet   (SExpr v) (SExpr (Maybe v))
  deriving (Functor)

seval :: Semiring d => (v -> d) -> SExpr v -> d
seval gen (SVar v)         =  gen v
seval gen SZero            =  zero
seval gen SOne             =  one
seval gen (SPlus   e1 e2)  =  seval gen e1  `plus`   seval gen e2
seval gen (STimes  e1 e2)  =  seval gen e1  `times`  seval gen e2
seval gen (SLet    e1 e2)  =  seval env e2 where
  env Nothing  = seval gen e1
  env (Just v) = gen v

-- modified abstractAD

sabstractAD :: Kronecker v d e => (v -> d) -> SExpr v -> CliffordWeil d e
sabstractAD var e = seval gen e where gen v = CW (var v) (delta v)

-- with extraction function

sabstractAD_extract :: Kronecker v d e => (v -> d) -> SExpr v -> e
sabstractAD_extract gen e = eCW $ sabstractAD gen e

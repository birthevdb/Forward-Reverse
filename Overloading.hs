{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Overloading where

import Data.Map

import Expressions
import Abstract
import Forward
import Reverse

-- simple (unary) case

type AbstractExpr1 = forall d. Semiring d => d -> d

abstractD1 :: Algebra d e => AbstractExpr1 -> d -> CliffordWeil d e
abstractD1 f d = f (CW d one)

-- x * (x + 1)
example1' :: AbstractExpr1
example1' d = times d (plus d one)

-- > eCW $ abstractD1 example1' 5
-- 11

-- general (n-ary) case

type AbstractExprn = forall d. Semiring d => [ d ] -> d

expr1ToExprn :: AbstractExpr1 -> AbstractExprn
expr1ToExprn e = h where h [ d ] = e d

exprnToExpr1 :: AbstractExprn -> AbstractExpr1
exprnToExpr1 e = h where h d = e [ d ]

-- x * y + x + 1
example2' :: AbstractExprn
example2' [x,y] = plus (plus (times x y) x) one

-- x * (x + 1) * (x + x)
example3' :: AbstractExprn
example3' [x]   = times x (times (plus x one) (plus x x))

abstract_reverseAD_n :: forall d. Semiring d => AbstractExprn -> [ d ] -> Map Int d
abstract_reverseAD_n e ds = (sparseSA . absEndo . absHom . eCW . e) [ CW d (delta i) | ((i :: Int) , d) <- zip [0..] ds ]

-- > abstract_reverseAD_n example2' [5,3]
-- fromList [(0,4),(1,5)]

-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE MonoLocalBinds #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE OverlappingInstances #-}

module Overloading where

-- import Data.Map
-- import Data.Array (Array,Ix,accumArray)
-- import Data.Array.IO
-- import Data.IORef
-- import Control.Monad (forM_)
-- import Control.Monad.State.Lazy
-- import Prelude hiding (lookup,map)


import Expressions


--
-- %if False
--
-- % rank-2 definition of Expr v; cf. Section 7.4
--
-- data EXPR v = EXPR (forall d. Semiring d => (v -> d) -> d)
--
-- expr_to_EXPR :: Expr v -> EXPR v
-- expr_to_EXPR e = EXPR (\ g -> eval g e)
--
-- expr_to_Expr :: EXPR v -> Expr v
-- expr_to_Expr (EXPR e) = e Var
--
-- %endif







-- %%%
-- %%% operator overloading cf. Section 7.4 \subsection{Naturality}
-- %%%
--
-- %%% simple (unary) case
--
-- type AbstractExpr1 = forall d. Semiring d =d -> d
--
-- abstractAD1 :: SAlgebra d e =AbstractExpr1 -> d -> CliffordWeil d e
-- abstractAD1 f d = f (CW d one)
--
-- example1' :: AbstractExpr1
-- example1' d = times d (plus d one)
--
-- < sndD $ abstractAD1 example1' 5
-- < 11
--
-- %%% general (n-ary) case: for n=1, kronecker 0 = one for the unique derivative
--
-- type AbstractExprn = forall d. Semiring d =[ d ] -> d
--
-- expr1ToExprn :: AbstractExpr1 -> AbstractExprn
-- expr1ToExprn e = h where h [ d ] = e d
--
-- exprnToExpr1 :: AbstractExprn -> AbstractExpr1
-- exprnToExpr1 e = h where h d = e [ d ]
--
-- example2' :: AbstractExprn
-- example2' [x,y] = plus (plus (times x y) x) one
--
-- example3' :: AbstractExprn
-- example3' [x]   = times x (times (plus x one) (plus x x))
--
-- %%% typechecker needs help now!
--
-- abstract_reverseAD_CY :: forall d. Semiring d =AbstractExprn -> [ d ] -> Map Int d
-- abstract_reverseAD_CY e ds =
-- >
--   sparseSA $ unE (unPW h (one :: d)) (mempty :: SparseSA Int d) where
-- >
--     CW _ h = e [ CW d (kronecker i) | ((i :: Int) , d) <- zip [0..] ds ]
--
-- < abstract_reverseAD_CY example2' [5,3]
-- < fromList [(0,4),(1,5)]
--

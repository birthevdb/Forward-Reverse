{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Streams where

import Expressions
import Abstract
import Forward

infixr 5 :<

data Stream d = d :< Stream d

-- isomorphism on the underlying data

cw_s :: CliffordWeil d (Stream d) -> Stream d
cw_s (CW d ds) = d :< ds

s_cw :: Stream d -> CliffordWeil d (Stream d)
s_cw (d :< ds) = CW d ds

-- instances

instance Semiring d => Semigroup (Stream d) where
  (x :< xs) <> (y :< ys)   = (x `plus` y) :< (xs <> ys)

instance Semiring d => Monoid (Stream d) where
  mempty = zero :< mempty

instance Semiring d => SModule d (Stream d) where
  d' `sact` (d :< ds) = (d' `times` d) :< (d' `sact` ds)

-- alternative account of Stream d differential structure

two :: Semiring d => d
two = one `plus` one

instance (Semiring d) => Semiring (Stream d) where
  zero  = cw_s zero
  one   = cw_s one
  xs `plus` ys   = xs <> ys
  (x :< xs) `times` (y :< ys) = z :< (zs `plus` (two `sact` (xs `times` ys)))
    where CW z zs = CW x xs `times` CW y ys


instance Semiring d => SAlgebra d (Stream d) where
  shom d = d :< zero

forwardAll :: (Eq v, Semiring d) => (v -> d) -> Expr v -> v -> Stream d
forwardAll gen e v = d :< (ds v) where CW d ds = abstractD gen e

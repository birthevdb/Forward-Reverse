module Symbolic where


import Expressions
import Abstract

------------------------------
-- Symbolic differentiation --
------------------------------

type ClassicalDual d = CliffordWeil d (SemiringAsSAlgebra d)

fstD :: Semiring d => ClassicalDual d -> d
fstD = dCW

sndD :: Semiring d => ClassicalDual d -> d
sndD = sa . eCW

derive :: (Eq v) => v -> Expr v -> ClassicalDual (Expr v)
derive x = eval gen where gen y = CW (Var y) (ddx y)
                          ddx y = if x == y then one else zero

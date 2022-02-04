{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE OverlappingInstances #-}

module Abstract where

import Expressions

---------------
-- Semirings --
---------------

class Semiring d where
  zero   :: d
  one    :: d
  plus   :: d -> d -> d
  times  :: d -> d -> d

-- numeric semiring instance

instance {-# OVERLAPPABLE #-} Num a => Semiring a where
  zero   = 0
  one    = 1
  plus   = (+)
  times  = (*)

-- the free semiring

instance Semiring (Expr v) where
  zero   = Zero
  one    = One
  plus   = Plus
  times  = Times

-- eval (because a fold) witnesses initiality/freeness

eval :: Semiring d => (v -> d) -> Expr v -> d
eval gen (Var x)        = gen x
eval gen Zero           = zero
eval gen One            = one
eval gen (Plus   e1 e2) = eval gen e1 `plus`  eval gen e2
eval gen (Times  e1 e2) = eval gen e1 `times` eval gen e2

---------------
-- d-modules --
---------------

-- if SModule d e, then we say that e is a d-module

class (Semiring d, Monoid e) => SModule d e | e -> d where
 sact :: d -> e -> e

----------------
-- d-algebras --
----------------

-- if e is d-module, and itself moreover a semiring,
-- then we speak of having a semiring algebra structure on e,
-- by analogy with rings/fields, and say that 'e is a d-algebra'

class (SModule d e, Semiring e) => SAlgebra d e where
  shom :: d -> e
  shom = (`sact` one)

-- an important special case: a semiring d is always a d-algebra

newtype SemiringAsSAlgebra d = SA { sa :: d } deriving (Functor, Show)

instance Semiring d => Semigroup (SemiringAsSAlgebra d) where
  (SA d) <> (SA d') = SA (d `plus` d')

instance Semiring d => Monoid (SemiringAsSAlgebra d) where
  mempty = SA zero

instance Semiring d => SModule d (SemiringAsSAlgebra d) where
  d `sact` (SA d') = SA (d `times` d')

instance Semiring d => Semiring (SemiringAsSAlgebra d) where
  zero = SA zero
  one  = SA one
  (SA d) `plus`  (SA d') = SA (d `plus`  d')
  (SA d) `times` (SA d') = SA (d `times` d')

instance {-# OVERLAPPABLE #-} Semiring d => SAlgebra d (SemiringAsSAlgebra d) where
  shom = SA

-------------------
-- Clifford-Weil --
-------------------

data CliffordWeil d e = CW { dCW :: d, eCW :: e } deriving Show

instance Functor (CliffordWeil d) where
  fmap h (CW d e) = CW d (h e)

-- the fundamental theorem: if e is a d-module, then CW d e is a semiring

instance SModule d e => Semiring (CliffordWeil d e) where
  zero                        = CW zero mempty
  one                         = CW one  mempty
  (CW f df) `plus`  (CW g dg) = CW (f `plus`  g) (df `mappend` dg)
  (CW f df) `times` (CW g dg) = CW (f `times` g) ((f `sact` dg) `mappend` (g `sact` df))

instance SModule d e => Semigroup (CliffordWeil d e) where
  (<>) = plus

instance SModule d e => Monoid (CliffordWeil d e) where
  mempty = zero

instance SModule d e => SModule d (CliffordWeil d e) where
  d' `sact` (CW d e) = CW (d' `times` d) (d' `sact` e)

instance SModule d e => SAlgebra d (CliffordWeil d e) where
  shom d = CW d mempty

---------------
-- Kronecker --
---------------

class SModule d e => Kronecker v d e where
  delta      :: v -> e
  delta v    = sdelta v (one :: d)
  sdelta     :: v -> d -> e
  sdelta v d = d `sact` (delta v)

-----------------
-- Abstract AD --
-----------------

abstractD :: Kronecker v d e => (v -> d) -> Expr v -> CliffordWeil d e
abstractD env = eval gen where gen v = CW (env v) (delta v)

-- with extraction function

abstractD_extract :: Kronecker v d e => (v -> d) -> Expr v -> e
abstractD_extract env = eCW . abstractD env

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Forward where

import Data.Map
import Prelude hiding (map)


import Expressions
import Abstract

-------------------------------
-- Classical forward-mode AD --
-------------------------------

type ClassicalDual d = CliffordWeil d (SemiringAsSAlgebra d)

fstD :: Semiring d => ClassicalDual d -> d
fstD = dCW

sndD :: Semiring d => ClassicalDual d -> d
sndD = sa . eCW

forwardAD :: (Eq v, Semiring d) => (v -> d) -> Expr v -> v -> d
forwardAD env exp v = sndD $ eval gen exp where
  gen w = CW (env w) (k w)
  k   w = if v == w then one else zero

-- < forwardAD (\X -> 5) example1 X
-- 11

--------------------------
-- Dense function space --
--------------------------

type Dense v e = v -> e

-- instances

instance SModule d e => SModule d (Dense v e) where
  d `sact` (f) = h where h v = d `sact` (f v)

instance (Eq v, SAlgebra d e) => Kronecker v d (Dense v e) where
  delta v = \ w -> if v == w then one else zero

-- forward AD

abstractforwardAD :: (Eq v, Semiring d) => (v -> d) -> Expr v -> v -> d
abstractforwardAD gen e = sa . (abstractD_extract gen e)

-- < abstractforwardAD (\X -> 5) example1 X
-- 11

---------------
-- Gradients --
---------------

-- inefficient version

type AllDual v d = Dense v (ClassicalDual d)

forwardGradient :: (Eq v, Semiring d) => (v -> d) -> Expr v -> AllDual v d
forwardGradient = abstractD_extract

-- with extraction function

forwardGradient_extract :: (Eq v, Semiring d) => (v -> d) -> Expr v -> v -> d
forwardGradient_extract gen e = dCW . forwardGradient gen e

-- > let e = let {env X1 = 5; env X2 = 3} in forwardGradient_extract env example2 in (e X1, e X2)
-- (4, 5)

-- optimized version

type DenseSA v d = Dense v (SemiringAsSAlgebra d)

abstractSharedGradient :: (Eq v, Semiring d) => (v -> d) -> Expr v -> DenseSA v d
abstractSharedGradient = abstractD_extract

-- with extraction function

abstractSharedGradient_extract :: (Eq v, Semiring d) => (v -> d) -> Expr v -> v -> d
abstractSharedGradient_extract gen e = sa . abstractSharedGradient gen e

-- < let e = let {env X1 = 5; env X2 = 3} in abstractSharedGradient_extract env example2 in (e X1, e X2)
-- (4, 5)

-----------------
-- Sparse maps --
-----------------

newtype Sparse v e = Sparse { sparse :: Map v e }

deriving instance (Ord v, Show v, Show e) => Show (Sparse v e)

sparseToDense :: forall v e. (Ord v, Monoid e) => Sparse v e -> Dense v e
sparseToDense (Sparse m) = f where f v = findWithDefault (mempty :: e) v m

-- instances

instance (Ord v, SModule d e) => Semigroup (Sparse v e) where
  (Sparse f) <> (Sparse g) = Sparse (unionWith (<>) f g)

instance (Ord v, SModule d e) => Monoid (Sparse v e) where
  mempty = Sparse empty

instance (Ord v, SModule d e) => SModule d (Sparse v e) where
  d `sact` (Sparse m) = Sparse $ map (d `sact`) m

instance (Ord v, SAlgebra d e) => Kronecker v d (Sparse v e) where
  delta v = Sparse $ singleton v one

-- this representation allows to write the following Sparse form of gradient computation

type SparseSA v d = Sparse v (SemiringAsSAlgebra d)

sparseSA :: SparseSA v d -> Map v d
sparseSA = map sa . sparse

-- forward AD

forwardADSparse :: (Ord v, Semiring d) => (v -> d) -> Expr v -> CliffordWeil d (Map v d)
forwardADSparse gen e = CW d (sparseSA m)
  where CW d m = abstractD gen e

-- with extraction function

forwardADSparse_extract :: (Ord v, Semiring d) => (v -> d) -> Expr v -> Map v d
forwardADSparse_extract gen = eCW . forwardADSparse gen

-- > let {env X1 = 5; env X2 = 3} in forwardADSparse_extract env example2
-- fromList [(X1,4),(X2,5)]

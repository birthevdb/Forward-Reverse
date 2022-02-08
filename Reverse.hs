{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Reverse where

import Data.Map
import Data.Array.IO
import Control.Monad (forM_)
import Control.Monad.State.Lazy
import Prelude hiding (map)
import Data.Array.Base (MArray(..))
import Control.Monad.Reader

import Expressions
import Abstract
import Forward

----------------------------------
-- Accumulating multiplications --
----------------------------------

newtype Hom d e = Hom { unHom :: d -> e }

reprHom :: SModule d e => e -> Hom d e
reprHom e = Hom (\d -> d `sact` e)

absHom :: SModule d e => Hom d e -> e
absHom (Hom f) = f one

-- instances

instance Semigroup e => Semigroup (Hom d e) where
  Hom f <> Hom g = Hom (\d -> f d <> g d)

instance Monoid e => Monoid (Hom  d e) where
  mempty = Hom (\d -> mempty)

instance SModule d e => SModule d (Hom d e) where
  d' `sact` (Hom f) = Hom (\d -> f (d' `times` d))

instance Kronecker v d e => Kronecker v d (Hom d e) where
  delta = Hom . sdelta

-- reverse mode AD

reverseAD :: (Ord v, Semiring d) => (v -> d) -> Expr v -> CliffordWeil d (Hom d (SparseSA v d))
reverseAD = abstractD

-- with extraction function

reverseAD_extract :: (Ord v, Semiring d) => (v -> d) -> Expr v -> Map v d
reverseAD_extract gen = sparseSA . absHom . eCW . reverseAD gen
-- TODO

-- example

-- > reverseAD_extract (\X -> 5) example3
-- fromList [(X,170)]

----------------------------
-- Accumulating additions --
----------------------------

newtype Endo e = E { unE :: e -> e }

reprEndo :: Monoid e => e -> Endo e
reprEndo e = E (\e' -> e' <> e)

absEndo :: Monoid e => Endo e -> e
absEndo (E f) = f mempty

-- instances

instance Semigroup (Endo e) where
  E f <> E g = E (g . f)

instance Monoid (Endo e) where
  mempty = E id

instance SModule d e => SModule d (Endo e) where
  d `sact` E f = E (\e -> f (d `sact` e))

instance Kronecker v d e => Kronecker v d (Endo e) where
  sdelta d v = E (\e -> e <> (sdelta d v))

reverseAD_Endo :: (Ord v, Semiring d) =>
  (v -> d) -> Expr v -> CliffordWeil d (Hom d (Endo (SparseSA v d)))
reverseAD_Endo = abstractD

-- with extraction function

reverseAD_Endo_extract :: (Ord v, Semiring d) => (v -> d) -> Expr v -> Map v d
reverseAD_Endo_extract gen = sparseSA . absEndo . absHom . eCW . reverseAD_Endo gen

-- example

-- > reverseAD_Endo_extract (\X -> 5) example3
-- fromList [(X,170)]

--------------------
-- Mutable arrays --
--------------------

newtype SM d m = SM { sm :: m () }

-- instances

instance Monad m => Semigroup (SM d m) where
  SM com <> SM com' = SM (com >> com')

instance Monad m => Monoid (SM d m) where
  mempty = SM $ return ()

-- reader monad combined with array monad

type MReadArray arr v e m = (Ix v, MArray arr e m, MonadReader (arr v e) m)

modifyArrayAt :: MReadArray arr v e m => (e -> e) -> v -> m ()
modifyArrayAt f v = do arr <- ask; a <- readArray arr v ; writeArray arr v (f a)

-- instances

instance (SAlgebra d e, MReadArray arr v e m) => SModule d (SM d m) where
  d `sact` com = SM $ do sm com; arr <- ask; b <- getBounds arr ; forM_ (range b) (modifyArrayAt (d `sact`))

instance (SAlgebra d e, MReadArray arr v e m) => Kronecker v d (SM d m) where
  sdelta v d = SM $ modifyArrayAt (`mappend` shom d) v

-- reverseAD

reverseAD_array :: (SAlgebra d e, MReadArray arr v e m)
                => (v -> d) -> Expr v -> CliffordWeil d (Hom d (SM d m))
reverseAD_array = abstractD

-- with extraction functions

reverseAD_array_extract  :: (SAlgebra d e, MReadArray arr v e m) => (v -> d) -> Expr v -> SM d m
reverseAD_array_extract gen = absHom . eCW . reverseAD_array gen

-- for IO

reverseAD_CY_IO_extract :: forall v d. (Ix v, Semiring d) => (v -> d) -> Expr v -> (v,v) -> IO (Map v d)
reverseAD_CY_IO_extract gen e rng = do (arr :: IOArray v (SemiringAsSAlgebra d)) <- newArray rng zero
                                       runReaderT (sm $ reverseAD_array_extract gen e) arr
                                       m <- getAssocs arr
                                       return $ map sa $ fromAscList m

instance MArray arr e m => MArray arr e (ReaderT x m) where
   getBounds = lift . getBounds
   getNumElements = lift . getNumElements
   unsafeRead arr i = lift (unsafeRead arr i)
   unsafeWrite arr i v = lift (unsafeWrite arr i v)

-- example

-- > reverseAD_CY_IO_extract (\X -> 5) example3 (X,X)
-- fromList [(X,170)]

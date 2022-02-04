{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE MonoLocalBinds #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE OverlappingInstances #-}

module Reverse where

import Data.Map
-- import Data.Array (Array,Ix,accumArray)
-- import Data.Array.IO
-- import Data.IORef
-- import Control.Monad (forM_)
-- import Control.Monad.State.Lazy
-- import Prelude hiding (lookup,map)

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

reverseAD :: (Ord v, Semiring d) => (v -> d) -> Expr v -> CliffordWeil d (d -> Map v d)
reverseAD gen e = CW d h where
  CW d (Hom f) = abstractD gen e
  h = sparseSA . f

-- with extraction function

-- reverseAD_extract :: (Ord v, Semiring d) => (v -> d) -> Expr v -> Map v d
-- reverseAD_extract gen e = absHom (Hom (eCW (reverseAD gen e))) -- h one where CW _ h = reverseAD gen e

-- > reverseAD_extract (\X -> 5) example3
-- fromList [(X,170)]

-- % we now consider a further optimisation of reverse mode AD:
-- % by passing from e to e -> e by Cayley/Yoneda-isation
-- % in the special case e = SparseSA v d
--
-- newtype Endo e = E { unE :: e -> e }
--
-- instance Semigroup (Endo e) where
--   E f <E g = E (g . f)
-- >
-- instance Monoid (Endo e) where
--   mempty = E id
--
-- instance SModule d e =SModule d (Endo e) where
-- >
--   d `sact` E f = E h where h e = f (d `sact` e)
--
-- instance Dirac v d e =Dirac v d (Endo e) where
--   dirac d v = E h where h e = e <(dirac d v)
--
-- reverseAD_CY :: (Ord v, Semiring d) =>
--   (v -> d) -> Expr v -> CliffordWeil d (PW d (Endo (SparseSA v d)))
-- reverseAD_CY env exp = abstractAD env exp
--
-- % with extraction function
--
-- reverseAD_CY_extract :: (Ord v, Semiring d) =(v -> d) -> Expr v -> Map v d
-- >
-- reverseAD_CY_extract env exp = sparseSA $ unE (unPW h one) mempty where
--   CW _ h = reverseAD_CY env exp
--
-- % example
--
-- < reverseAD_CY_extract (\X -> 5) example3
-- < fromList [(X,170)]
--
-- %%%
-- %%% moving to imperative implementation:
-- %%% from d -> SparseSA v d -> SparseSA v d to d -> IO()
-- %%%
--
-- %%% fixed by Tom 2021-11-18; abstract Array tweaks by James
--
-- %%% firstly, set up Monoid structure on m ()
--
-- newtype SM d m = SM { sm :: m () } -- to maintain FunctionalDependencies
--
-- instance Monad m =Semigroup (SM d m) where
-- >
--   (SM com) <(SM com') = SM (com >com')
--
-- instance Monad m =Monoid (SM d m) where
-- >
--   mempty = SM $ return ()
--
-- %%% second, go via State e () : what's the d-module structure???
--
-- instance SModule d e =SModule d (SM d (State e)) where
-- >
--   d `sact` (SM com) = SM $ modify (d `sact`) >com
--
-- instance Dirac v d e =Dirac v d (SM d (State e)) where
-- >
--   dirac v d = SM $ modify (<dirac v d)
--
-- % hence obtain pure state implementation
--
-- reverseAD_CY_state :: (Ord v, Semiring d) =>
--   (v -> d) -> Expr v -> CliffordWeil d (PW d (SM d (State (SparseSA v d))))
-- reverseAD_CY_state env exp = abstractAD env exp
--
-- reverseAD_CY_state_extract :: (Ord v, Semiring d) =>
--   (v -> d) -> Expr v -> Map v d
-- reverseAD_CY_state_extract env exp = sparseSA $ execState (sm (unPW h one)) mempty where
--   CW _ h = reverseAD_CY_state env exp
--
-- %%% hence, as before
--
-- < reverseAD_CY_state_extract (\X -> 5) example3
-- < fromList [(X,170)]
--
-- %%% thence to IO, using an MArray to store the (Sparse v d) state
--
-- modifyArrayAt :: (Ix v, MArray a e m) =(e -> e) -> a v e -> v -> m ()
-- modifyArrayAt f arr v = do a <- readArray arr v ; writeArray arr v (f a)
--
-- modifyArray :: (Ix v, MArray a e m) =(e -> e) -> a v e -> m ()
-- modifyArray f arr = do b <- getBounds arr ; forM_ (range b) $ modifyArrayAt f arr
--
-- instance (SAlgebra d e, Ix v, MArray a e m) =Dirac v d (a v e -> SM d m) where
-- >
--   dirac d v arr = SM $ modifyArrayAt (<shom d) arr v
--
-- instance (SAlgebra d e, Ix v, MArray a e m) =SModule d (a v e -> SM d m) where
-- >
--   (d `sact` com) arr = SM $ do (sm $ com arr) ; modifyArray (d `sact`) arr
--
-- reverseAD_CY_arrayM :: (SAlgebra d e, Ix v, MArray a e m) =>
--   (v -> d) -> Expr v -> CliffordWeil d (PW d (a v e -> SM d m))
-- >
-- reverseAD_CY_arrayM env exp = abstractAD env exp
--
-- reverseAD_CY_IO_extract :: forall v d. (Ix v, Semiring d) =>
--   (v -> d) -> Expr v -> (v,v) -> IO (Map v d)
-- >
-- reverseAD_CY_IO_extract env exp rng =
-- >
--   do (arr :: IOArray v (SemiringAsSAlgebra d)) <- newArray rng zero
--      sm $ unPW h one arr
--      m <- getAssocs arr
--      return $ map sa $ fromAscList m
-- >
--   where CW _ h = reverseAD_CY_arrayM env exp
--
-- %%% hence, as before
--
-- < reverseAD_CY_IO_extract (\X -> 5) example3 rangeX
-- < fromList [(X,170)]
--

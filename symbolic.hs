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
-- {-# LANGUAGE TypeSynonymInstances #-}

module Symbolic where

-- import Data.Map
-- import Data.Array (Array,Ix,accumArray)
-- import Data.Array.IO
-- import Data.IORef
-- import Control.Monad (forM_)
-- import Control.Monad.State.Lazy
-- import Prelude hiding (lookup,map)


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

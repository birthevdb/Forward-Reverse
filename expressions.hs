{-# LANGUAGE DeriveFunctor #-}

module Expressions where

import Data.Array (Ix)
import Data.Array.IO

-----------------
-- Expressions --
-----------------

data Expr v
  =  Var v
  |  Zero
  |  One
  |  Plus   (Expr v) (Expr v)
  |  Times  (Expr v) (Expr v)
  deriving Functor


instance Show a => Show (Expr a) where
 showsPrec p (Var x)        =  showsPrec p x
 showsPrec p Zero           =  shows 0
 showsPrec p One            =  shows 1
 showsPrec p (Plus e1 e2)   =  showParen (p >= 6) $ (showsPrec 6 e1) . (" + " ++) . (showsPrec 6 e2)
 showsPrec p (Times e1 e2)  =  showParen (p >= 7) $ (showsPrec 7 e1) . (" * " ++) . (showsPrec 7 e2)

-- example 1

data X = X
   deriving (Show, Eq, Ord)

instance Ix X where
  range (X, X) = [X]
  inRange (X, X) X = True
  index (X, X) X = 0

rangeX :: (X,X)
rangeX = (X,X)

example1 :: Expr X
example1 = Times (Var X) (Plus (Var X) One)

-- example 2

data XX = X1 | X2
   deriving (Show, Eq, Ord)

example2 :: Expr XX
example2 = Plus (Plus (Times (Var X1) (Var X2)) (Var X1)) One

-- example 3

example3 :: Expr X
example3 = Times (Var X) (Times (Plus (Var X) One) (Plus (Var X) (Var X)))

{-# LANGUAGE NoImplicitPrelude, UnboxedTuples #-}

module Alt.Algebra
  ( 
    Semigroup(..)
  , Monoid(..)
  , Group(..)
  , Semiring(..)
  , Monoring(..)
  , Ring(..)
  , Integral(..)
  , Division(..)
  , Field(..)
  ) where

class Semigroup g where
  (<>) :: g -> g -> g

class Monoid m where
  (++) :: m -> m -> m
  empty :: m

class Group m where
  (&) :: m -> m -> m
  inv :: m -> m
  identity :: m

class Semiring r where
  (+) :: r -> r -> r
  (*) :: r -> r -> r

class Monoring r where
  zero :: r
  one :: r

class Semiring r => Ring r where
  (-) :: r -> r -> r

class Ring r => Integral r where
  div :: r -> r -> r
  mod :: r -> r -> r
  divMod :: r -> r -> (r, r)
  divMod a b = (div a b, mod a b)

class Ring r => Division r where
  (/) :: r -> r -> r

class Division r => Field r

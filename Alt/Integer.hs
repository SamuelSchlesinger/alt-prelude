{-# LANGUAGE RebindableSyntax, UnboxedTuples #-}

module Alt.Integer
  ( 
    Integer
  , FromInteger(..)
  ) where

import Alt.Category
import Alt.Algebra

import GHC.Integer

class FromInteger i where
  fromInteger :: Integer -> i

instance FromInteger Integer where
  fromInteger = id

instance Semiring Integer where
  (+) = plusInteger
  (*) = timesInteger

instance Monoring Integer where
  one = 1
  zero = 0

instance Ring Integer where
  (-) = minusInteger

instance Integral Integer where
  div = divInteger
  mod = modInteger
  divMod a b = let (# d, m #) = divModInteger a b in (d, m)

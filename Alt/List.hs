{-# LANGUAGE NoImplicitPrelude #-}

module Alt.List where

import Alt.Algebra

instance Semigroup [a] where
  [] <> b = b
  (x:xs) <> b = x : (xs <> b)

instance Monoid [a] where
  (++) = (<>)
  empty = []

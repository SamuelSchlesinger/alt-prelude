{-# LANGUAGE TypeFamilies #-}

module Alt.Vector
  (
    Vector(..)
  , module Data.Vector
  ) where

import Data.Vector
import Alt.Category
import Alt.Constraint
import Alt.Functor
import Alt.Monad
import qualified Prelude as P

instance Functor Vector where
  type Dom Vector = (->)
  type Cod Vector = (->)
  map = P.fmap
  constrained = Sub Dict

instance Pure Vector where
  pure = singleton

instance Monad Vector where
  bind = P.flip (P.>>=)

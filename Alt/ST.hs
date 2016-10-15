{-# LANGUAGE TypeFamilies #-}

module Alt.ST
  (
    module Control.Monad.ST
  , module Data.STRef
  ) where

import qualified Prelude as P
import Control.Monad.ST
import Data.STRef
import Alt.Functor
import Alt.Pure
import Alt.Constraint
import Alt.Applicative
import Alt.Monad

instance Functor (ST s) where
  type Dom (ST s) = (->)
  type Cod (ST s) = (->)
  map = P.fmap
  constrained = Sub Dict

instance Pure (ST s) where
  pure = P.return

instance Monad (ST s) where
  bind = P.flip (P.>>=)

instance Applicative (ST s) where
  apply = (P.<*>)

{-# LANGUAGE NoImplicitPrelude #-}

module Alt.Applicative
  (
    Applicative(..)
  , Alternative(..)
  , (<*>)
  ) where

import Alt.Functor
import Alt.Pure

class (Functor f, Pure f) => Applicative f where
  apply :: f (a -> b) -> f a -> f b

(<*>) :: Applicative f => f (a -> b) -> f a -> f b
(<*>) = apply

class (Functor f, Pure f) => Alternative f where
  (<|>) :: f a -> f a -> f a
  nop :: f a 

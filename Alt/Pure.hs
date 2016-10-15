{-# LANGUAGE NoImplicitPrelude #-}

module Alt.Pure
  ( 
    Pure(..)
  ) where

import Alt.Functor
import Alt.Maybe
import Alt.Either
import Alt.IO

class Functor f => Pure f where
  pure :: a -> f a

instance Pure Maybe where
  pure = Just

instance Pure IO where
  pure = returnIO

instance Pure [] where
  pure = (:[])

instance Pure (Either a) where
  pure = Right

{-# LANGUAGE RebindableSyntax, LambdaCase, NoImplicitPrelude, TypeFamilies, PolyKinds, TypeOperators, DataKinds, TypeInType, GADTs, FlexibleInstances, UndecidableInstances, AllowAmbiguousTypes #-}

module Alt.Monad
  ( 
    Monad(..)
  , (>>=)
  , return
  ) where

import Alt.Proxy
import Alt.Functor
import Alt.Type
import Alt.Pure
import Alt.Maybe
import Alt.Either
import Alt.Category
import Alt.IO

class (Endo f, Pure f) => Monad f where
  bind :: Dom f a (f b) -> Cod f (f a) (f b)

return :: Monad f => a -> f a
return = pure
(>>=) :: Monad f => f a -> (a -> f b) -> f b
a >>= f = f `bind` a

(=<<) :: Monad f => (a -> f b) -> f a -> f b
(=<<) = bind

(>=>) :: Monad f => (a -> f b) -> (b -> f c) -> a -> f c
(f >=> g) a = f a >>= g

(<=<) :: Monad f => (b -> f c) -> (a -> f b) -> a -> f c
(f <=< g) a = f =<< g a

instance Monad IO where
  bind = bindIO

instance Monad Maybe where
  bind f = \case
    Just x -> f x
    Nothing -> Nothing

instance Monad (Either a) where
  bind f = \case
    Right x -> f x
    Left x -> Left x

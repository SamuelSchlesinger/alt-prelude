{-# LANGUAGE 
    ConstraintKinds
  , DataKinds
  , PolyKinds
  , RankNTypes
  , TypeInType
  , TypeFamilies
  , FlexibleContexts
  , NoImplicitPrelude 
  , TypeOperators 
  , AllowAmbiguousTypes
  , LambdaCase 
  , DefaultSignatures #-}

module Alt.Functor
  ( 
    Functor(..)
  , Endo
  ) where

import Alt.Constraint
import Alt.Type
import Alt.Maybe
import Alt.Either
import Alt.Category
import Alt.IO

class (Category (Dom f), Category (Cod f)) => Functor f where
  type Dom f :: i -> i -> Type
  type Cod f :: j -> j -> Type
  map :: Dom f x y -> Cod f (f x) (f y)
  constrained :: forall a. (Dom f |^ a) :- (Cod f |^ f a)

type Endo f = (Functor f, Dom f ~ Cod f)

(<$>) :: (Functor f, Cod f ~ (->)) => Dom f a b -> f a -> f b
(<$>) = map

instance Functor Either where
  type Dom Either = (->)
  type Cod Either = P1
  map f = P1 (\case
    Left a -> Left (f a)
    Right b -> Right b)
  constrained = Sub Dict

instance Functor (Either a) where
  type Dom (Either a) = (->)
  type Cod (Either a) = (->)
  map f = \case
    Left a -> Left a
    Right b -> Right (f b)
  constrained = Sub Dict

instance Functor Maybe where
  type Dom Maybe = (->)
  type Cod Maybe = (->)
  map f m = case m of
    Just x -> Just (f x)
    Nothing -> Nothing
  constrained = Sub Dict

instance Functor IO where
  type Dom IO = (->)
  type Cod IO = (->)
  map = mapIO
  constrained = Sub Dict

instance Functor [] where
  type Dom [] = (->)
  type Cod [] = (->)
  map f = \case
    [] -> []
    x : xs -> f x : map f xs
  constrained = Sub Dict 

instance Functor ((,) a) where
  type Dom ((,) a) = (->)
  type Cod ((,) a) = (->)
  map f (a, b) = (a, f b)
  constrained = Sub Dict

instance Functor (,) where
  type Dom (,) = (->)
  type Cod (,) = P1
  map f = P1 (\(a, b) -> (f a, b))
  constrained = Sub Dict

instance Functor ((,,) a) where
  type Dom ((,,) a) = (->)
  type Cod ((,,) a) = P1
  map f = P1 (\(a, b, c) -> (a, f b, c))
  constrained = Sub Dict

instance Functor (,,) where
  type Dom (,,) = (->)
  type Cod (,,) = P2
  map f = P2 (\(a, b, c) -> (f a, b, c))
  constrained = Sub Dict

instance Functor ((,,,) a b c) where
  type Dom ((,,,) a b c) = (->)
  type Cod ((,,,) a b c) = (->)
  map f (a, b, c, d) = (a, b, c, f d)
  constrained = Sub Dict

instance Functor ((,,,) a b) where
  type Dom ((,,,) a b) = (->)
  type Cod ((,,,) a b) = P1
  map f = P1 (\(a, b, c, d) -> (a, b, f c, d))
  constrained = Sub Dict

instance Functor ((,,,) a) where
  type Dom ((,,,) a) = (->)
  type Cod ((,,,) a) = P2
  map f = P2 (\(a, b, c, d) -> (a, f b, c, d))
  constrained = Sub Dict

instance Functor (,,,) where
  type Dom (,,,) = (->)
  type Cod (,,,) = P3
  map f = P3 (\(a, b, c, d) -> (f a, b, c, d))
  constrained = Sub Dict

instance Functor ((,,,,) a b c d) where
  type Dom ((,,,,) a b c d) = (->)
  type Cod ((,,,,) a b c d) = (->)
  map f (a, b, c, d, e) = (a, b, c, d, f e)
  constrained = Sub Dict

instance Functor ((,,,,) a b c) where
  type Dom ((,,,,) a b c) = (->)
  type Cod ((,,,,) a b c) = P1
  map f = P1 (\(a, b, c, d, e) -> (a, b, c, f d, e))
  constrained = Sub Dict

instance Functor ((,,,,) a b) where
  type Dom ((,,,,) a b) = (->)
  type Cod ((,,,,) a b) = P2
  map f = P2 (\(a, b, c, d, e) -> (a, b, f c, d, e))
  constrained = Sub Dict

instance Functor ((,,,,) a) where
  type Dom ((,,,,) a) = (->)
  type Cod ((,,,,) a) = P3
  map f = P3 (\(a, b, c, d, e) -> (a, f b, c, d, e))
  constrained = Sub Dict

instance Functor (,,,,) where
  type Dom (,,,,) = (->)
  type Cod (,,,,) = P4
  map f = P4 (\(a, b, c, d, e) -> (f a, b, c, d, e))
  constrained = Sub Dict

instance Functor ((,,,,,) a b c d e) where
  type Dom ((,,,,,) a b c d e) = (->)
  type Cod ((,,,,,) a b c d e) = (->)
  map f (a, b, c, d, e, x) = (a, b, c, d, e, f x)
  constrained = Sub Dict

instance Functor ((,,,,,) a b c d) where
  type Dom ((,,,,,) a b c d) = (->)
  type Cod ((,,,,,) a b c d) = P1
  map f = P1 (\(a, b, c, d, e, x) -> (a, b, c, d, f e, x))
  constrained = Sub Dict

instance Functor ((,,,,,) a b c) where
  type Dom ((,,,,,) a b c) = (->)
  type Cod ((,,,,,) a b c) = P2
  map f = P2 (\(a, b, c, d, e, x) -> (a, b, c, f d, e, x))
  constrained = Sub Dict

instance Functor ((,,,,,) a b) where
  type Dom ((,,,,,) a b) = (->)
  type Cod ((,,,,,) a b) = P3
  map f = P3 (\(a, b, c, d, e, x) -> (a, b, f c, d, e, x))
  constrained = Sub Dict

instance Functor ((,,,,,) a) where
  type Dom ((,,,,,) a) = (->)
  type Cod ((,,,,,) a) = P4
  map f = P4 (\(a, b, c, d, e, x) -> (a, f b, c, d, e, x))
  constrained = Sub Dict

instance Functor (,,,,,) where
  type Dom (,,,,,) = (->)
  type Cod (,,,,,) = P5
  map f = P5 (\(a, b, c, d, e, x) -> (f a, b, c, d, e, x))
  constrained = Sub Dict

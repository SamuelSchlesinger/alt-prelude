{-# LANGUAGE TypeFamilies, PolyKinds, RankNTypes, DataKinds, TypeInType, GADTs, ScopedTypeVariables, RebindableSyntax, ConstraintKinds, TypeOperators, DefaultSignatures #-}

module Algebra 
  (
    Hom
  , Nat(..)
  , Cat(..)
  , Category(..)
  , Functor(..)
  , Monad(..), (>>), (<<), (=<<)
  , Semiring(..)
  , Ring(..)
  , Division(..)
  , Integral(..)
  , Semigroup(..)
  , Monoid(..)
  , Group(..)
  , Type
  , Integer
  , Z
  , Double
  , Float
  , Int
  , Show(..)
  , Eq(..)
  , Read(..)
  , IO
  , Mod
  , print
  , Number(..)
  , Fraction(..)
  , (:-)
  ) where

import Data.Kind
import Data.Proxy
import Data.Bool
import GHC.Exts
import Data.Maybe
import qualified GHC.TypeLits as T
import Prelude (Integer, Int, Double, Float, Show(..), Eq(..), Read(..), IO, print)
import qualified Prelude as P

type Z = Integer

type family Hom :: i -> i -> Type

newtype Nat f g = Nat { runNat :: forall x. f x -> g x }

newtype Cat c d = Cat { runCat :: forall x y. c x y -> d x y }

data Dict :: Constraint -> Type where
  Dict :: a => Dict a

newtype c :- d = Sub (c => Dict d)

(\\) :: a => (b => d) -> (a :- b) -> d
d \\ Sub Dict = d

type instance Hom = (:-)
type instance Hom = (->)
type instance Hom = Nat
type instance Hom = Cat

type family (|^) (c :: (i -> i -> Type)) (x :: i) :: Constraint
type instance (:-) |^ x = ()
type instance (->) |^ x = ()
type instance Nat  |^ f = ()
type instance Cat  |^ c = ()

class c ~ Hom => Category c where
  id :: c |^ x => c x x
  (.) :: c y z -> c x y -> c x z
  expose :: c y z -> Dict (c |^ y, c |^ z)

instance Category (:-) where
  id = Sub Dict
  f . g = Sub (Dict \\ f \\ g)
  expose a = Dict

instance Category (->) where
  id x = x
  (f . g) x = f (g x)
  expose a = Dict

instance Category Nat where
  id = Nat id
  Nat f . Nat g = Nat (f . g)
  expose a = Dict

instance Category Cat where
  id = Cat id
  f . g = Cat (runCat f . runCat g)
  expose a = Dict

class Functor f where
  type Dom f :: i -> i -> Type
  type Cod f :: j -> j -> Type
  map :: Dom f x y -> Cod f (f x) (f y)

class Functor f => Monad f where
  (>>=) :: f a -> (a -> f b) -> f b
  return :: a -> f a

(=<<) :: Monad f => (a -> f b) -> f a -> f b
(=<<) = P.flip (>>=)

(>>) :: Monad f => f a -> f b -> f b
a >> b = a >>= \_ -> b

(<<) :: Monad f => f a -> f b -> f a
a << b = (\_ -> a) =<< b

instance Functor IO where
  type Dom IO = (->)
  type Cod IO = (->)
  map = P.fmap

instance Monad IO where
  (>>=) = (P.>>=)
  return = P.return

instance Functor [] where
  type Dom [] = (->)
  type Cod [] = (->)
  map f xs = case xs of
    [] -> []
    x : xs -> f x : map f xs

instance Functor (,) where
  type Dom (,) = (->)
  type Cod (,) = Nat
  map f = Nat (\(a, b) -> (f a, b))

instance Functor ((,) a) where
  type Dom ((,) a) = (->)
  type Cod ((,) a) = (->)
  map f (a, b) = (a, f b)

class Monoid m where
  (++) :: m -> m -> m
  empty :: m

instance Monoid [a] where
  (++) = (P.++)
  empty = []

class Semigroup g where
  (<>) :: g -> g -> g
  
class Group g where
  (&) :: g -> g -> g
  inv :: g -> g
  identity :: g

class Semiring r where
  (+) :: r -> r -> r
  (*) :: r -> r -> r
  one :: r
  zero :: r

class Semiring r => Ring r where
  (-) :: r -> r -> r

class Ring r => Integral r where
  div :: r -> r -> r
  mod :: r -> r -> r

class Ring r => Division r where
  (/) :: r -> r -> r

instance Semiring Double where
  (+) = (P.+)
  (*) = (P.*)
  one = 1
  zero = 0

instance Ring Double where
  (-) = (P.-)

instance Division Double where
  (/) = (P./)

instance Semiring Integer where
  (+) = (P.+)
  (*) = (P.*)
  one = 1
  zero = 0

instance Ring Integer where
  (-) = (P.-)

instance Integral Integer where
  div = P.div
  mod = P.mod  

instance Semiring Int where
  (+) = (P.+)
  (*) = (P.*)
  one = 1
  zero = 0

instance Ring Int where
  (-) = (P.-)

instance Integral Int where
  div = P.div
  mod = P.mod

instance Semiring Float where
  (+) = (P.+)
  (*) = (P.*)
  one = 1
  zero = 0

instance Ring Float where
  (-) = (P.-)

instance Division Float where
  (/) = (P./)

data Mod :: Type -> T.Nat -> Type where
  Mod :: i -> Mod i n

instance (Show i, Number i, Integral i, T.KnownNat n) => Show (Mod i n) where
  show (Mod n) = show (n `mod` fromInteger (T.natVal (Proxy :: Proxy n)))

class Ring n => Number n where
  fromInteger :: Integer -> n

instance Number Double where
  fromInteger = P.fromInteger

instance Number Integer where
  fromInteger = id

instance Number Float where
  fromInteger = P.fromInteger

instance Number Int where
  fromInteger = P.fromInteger

class Number n => Fraction n where
  fromRational :: P.Rational -> n

instance Fraction Float where
  fromRational = P.fromRational

instance Fraction Double where
  fromRational = P.fromRational

instance (Number i, Integral i, T.KnownNat n) => Semiring (Mod i n) where
  Mod a + Mod b = Mod ((a + b) `mod` fromInteger (T.natVal (Proxy :: Proxy n)))
  Mod a * Mod b = Mod ((a * b) `mod` fromInteger (T.natVal (Proxy :: Proxy n)))
  one = Mod 1
  zero = Mod 0

instance (Number i, Integral i, T.KnownNat n) => Ring (Mod i n) where
  Mod a - Mod b = Mod ((a - b) `mod` fromInteger (T.natVal (Proxy :: Proxy n)))

instance (Integral i, Number i, T.KnownNat n) => Number (Mod i n) where
  fromInteger = Mod . fromInteger

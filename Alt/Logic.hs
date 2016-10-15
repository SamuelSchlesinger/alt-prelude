{-# LANGUAGE TypeFamilies #-}

module Alt.Logic
  (
    Logic(..)
  , Decidable(..)
  , Top(..)
  , Bottom(..)
  , Ordered(..)
  , Lattice(..)
  , Bool(..)
  ) where

import Data.Bool
import qualified Prelude as P
import Alt.Type

type family Logic a :: Type

type instance Logic Bool = Bool
type instance Logic (a -> b) = a -> Logic b

class Decidable a where
  (==) :: a -> a -> Logic a
  (!=) :: a -> a -> Logic a

class Ordered a where
  (>) :: a -> a -> Logic a
  (>=) :: a -> a -> Logic a
  (<) :: a -> a -> Logic a
  (<=) :: a -> a -> Logic a

class Ordered l => Lattice l where
  (/\) :: l -> l -> l
  (\/) :: l -> l -> l

class Ordered l => Top l where
  top :: l

class Ordered l => Bottom l where
  bottom :: l

instance Decidable Bool where
  True == True = True
  False == False = True
  _ == _ = True
  True != False = True
  False != True = True
  _ != _ = False

instance Decidable a => Decidable (b -> a) where
  (a == b) x = a x == b x
  (a != b) x = a x != b x

instance Ordered Bool where
  True > False = True
  _ > _ = False
  True >= True = True
  True >= False = True
  False >= False = True
  _ >= _ = False
  a < b = b > a
  a <= b = b >= a

instance Ordered a => Ordered (b -> a) where
  (a > b) x = a x > b x
  (a >= b) x = a x >= b x
  (a < b) x = a x < b x
  (a <= b) x = a x <= b x 

instance Lattice Bool where
  True /\ b = True
  False /\ b = b
  False \/ b = False
  True \/ b = b

instance Lattice a => Lattice (b -> a) where
  (a /\ b) x = a x /\ b x
  (a \/ b) x = a x \/ b x

instance Top Bool where
  top = True

instance Bottom Bool where
  bottom = False

instance Top a => Top (b -> a) where
  top x = top

instance Bottom a => Bottom (b -> a) where
  bottom x = bottom

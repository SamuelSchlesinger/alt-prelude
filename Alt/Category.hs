{-# LANGUAGE PolyKinds, TypeFamilies, TypeInType, TypeOperators, RankNTypes, ConstraintKinds, NoImplicitPrelude #-}

module Alt.Category
  ( 
    Hom
  , type (|^)
  , Category(..)
  , P1(..), P2(..), P3(..), P4(..), P5(..)
  ) where

import Alt.Type
import Alt.Constraint

-- | We have room for a category for every kind
type family Hom :: i -> i -> Type

-- | Some categories may be constrained in some way or another
type family (|^) (cat :: i -> i -> Type) (obj :: i) :: Constraint

-- | We can now construct a separate category for each
-- kind which is being used as an instance of Hom.
class Hom ~ c => Category c where
  (.) :: c y z -> c x y -> c x z
  id  :: c |^ x => c x x
  expose :: c x y -> Dict (c |^ x, c |^ y)

type instance Hom = (:-)
type instance (:-) |^ c = ()

instance Category (:-) where
  f . g = trans g f
  id = refl
  expose _ = Dict
  
type instance Hom = (->)
type instance (->) |^ a = ()
 
instance Category (->) where
  (f . g) x = f (g x)
  id x = x
  expose _ = Dict

newtype P1 f g = P1 { runP1 :: forall a. f a -> g a }
newtype P2 f g = P2 { runP2 :: forall a b. f a b -> g a b }
newtype P3 f g = P3 { runP3 :: forall a b c. f a b c -> g a b c }
newtype P4 f g = P4 { runP4 :: forall a b c d. f a b c d -> g a b c d }
newtype P5 f g = P5 { runP5 :: forall a b c d e. f a b c d e -> g a b c d e }
newtype P6 f g = P6 { runP6 :: forall a b c d e x. f a b c d e x -> g a b c d e x }

type instance Hom = P1
type instance Hom = P2
type instance Hom = P3
type instance Hom = P4
type instance Hom = P5
type instance Hom = P6
type instance P1 |^ x = ()
type instance P2 |^ x = ()
type instance P3 |^ x = ()
type instance P4 |^ x = ()
type instance P5 |^ x = ()
type instance P6 |^ x = ()

instance Category P1 where
  id = P1 id
  f . g = P1 (runP1 f . runP1 g)
  expose _ = Dict

instance Category P2 where
  id = P2 id
  f . g = P2 (runP2 f . runP2 g)
  expose _ = Dict

instance Category P3 where
  id = P3 id
  f . g = P3 (runP3 f . runP3 g)
  expose _ = Dict

instance Category P4 where
  id = P4 id
  f . g = P4 (runP4 f . runP4 g)
  expose _ = Dict

instance Category P5 where
  id = P5 id
  f . g = P5 (runP5 f . runP5 g)
  expose _ = Dict

instance Category P6 where
  id = P6 id
  f . g = P6 (runP6 f . runP6 g)
  expose _ = Dict

{-# LANGUAGE PolyKinds, ConstraintKinds, TypeFamilies, GADTs, TypeOperators, RankNTypes #-}

module Alt.Constraint
  ( 
    Constraint
  , Dict(..)
  , type (:-)(..)
  , (\\)
  , trans
  , refl
  ) where

import Data.Kind

data Dict a where
  Dict :: a => Dict a

newtype a :- b = Sub (a => Dict b)

(\\) :: a => (b => c) -> a :- b -> c
d \\ Sub Dict = d

trans :: a :- b -> b :- c -> a :- c
trans ab bc = Sub $ Dict \\ bc \\ ab

refl :: a :- a
refl = Sub Dict



module Alt.Concrete
  ( 
    Concrete(..)
  ) where

import Alt.Category

class Category c => Concrete c where
  type Realize c x :: Type
  ($) :: c x y -> Realize c x -> Realize c y

infixr 0 $

instance Concrete (->) where
  type Realize (->) x = x
  f $ x = f x

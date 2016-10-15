{-# LANGUAGE NoImplicitPrelude, KindSignatures, TypeFamilies, DataKinds, GADTs, ConstraintKinds, PolyKinds #-}

module Alt.Constrained
  (
    CFun(..)
  , ConstrainedType(..)
  ) where

import qualified Prelude
import Alt.Category
import Unsafe.Coerce
import Alt.Type
import Alt.Constraint

data ConstrainedType = CTy (Type -> Constraint) Type

data CFun :: ConstrainedType -> ConstrainedType -> Type where
  CFun :: (c a, c b) => (a -> b) -> CFun (CTy c a) (CTy c b) 

type instance Hom = CFun
type instance CFun |^ CTy c a = c a

instance Category CFun where
  CFun f . CFun g = CFun (f . g)
  id = Prelude.undefined -- CFun id
  expose (CFun f) = Dict

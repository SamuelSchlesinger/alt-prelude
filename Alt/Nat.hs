{-# LANGUAGE DataKinds, ExplicitNamespaces, ScopedTypeVariables #-}

module Alt.Nat
  ( 
    Nat
  , SomeNat(..)
  , someNatVal
  , natVal
  , KnownNat(..)
  , type (<=)
  , type (<=?) 
  , type (+)
  , type (*)
  , type (^)
  , type (-)
  , type CmpNat
  ) where

import qualified GHC.TypeLits as T
import Alt.Proxy
import GHC.TypeLits hiding (natVal)
import Alt.Integer

natVal :: (FromInteger i, KnownNat n) => Proxy n -> i
natVal (p :: Proxy n) = Alt.Integer.fromInteger (T.natVal p)

{-# LANGUAGE NoImplicitPrelude, DataKinds, TypeInType, GADTs, ScopedTypeVariables, RebindableSyntax, TypeFamilies #-}

module Alt.Mod
  ( 
    Mod
  , Z
  , Aleph0
  ) where

import Alt.Algebra
import Alt.Proxy
import Alt.Type
import Alt.Integer
import Alt.Nat
import Prelude (Show(..))

data Aleph0

type family Z (n :: i) :: Type where
  Z Aleph0 = Integer
  Z n = Mod Integer n

data Mod (i :: Type) (n :: Nat) :: Type where
  Mod :: i -> Mod i n

instance (Integral i, KnownNat n, FromInteger i) => FromInteger (Mod i n) where
  fromInteger i = Mod (fromInteger (i `mod` natVal (Proxy :: Proxy n)))

instance (Show i, Integral i, KnownNat n, FromInteger i) => Show (Mod i n) where
  show (Mod i) = show (i `mod` natVal (Proxy :: Proxy n))

instance (Integral i, KnownNat n, FromInteger i) => Semiring (Mod i n) where
  ((Mod a) :: Mod i n) + Mod b = Mod ( (a + b) `mod` natVal (Proxy :: Proxy n) )
  ((Mod a) :: Mod i n) * Mod b = Mod ( (a * b) `mod` natVal (Proxy :: Proxy n) )

instance (Integral i, KnownNat n, FromInteger i) => Monoring (Mod i n) where
  one = Mod 1
  zero = Mod 0

instance (Integral i, KnownNat n, FromInteger i) => Ring (Mod i n) where
  ((Mod a) :: Mod i n) - Mod b = Mod ( (a - b) `mod` natVal (Proxy :: Proxy n))

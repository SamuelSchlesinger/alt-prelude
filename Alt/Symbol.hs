{-# LANGUAGE ExplicitNamespaces #-}

module Alt.Symbol
  ( 
    Symbol
  , SomeSymbol(..)
  , KnownSymbol(..)
  , someSymbolVal
  , sameSymbol
  , type CmpSymbol
  ) where

import GHC.TypeLits

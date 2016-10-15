{-# LANGUAGE UnboxedTuples, RebindableSyntax, TemplateHaskell, TypeFamilies #-}

module Alt.Int
  ( 
    Int, Int64, Int32, Int16, Int8
  ) where

import Data.Int
import Alt.Algebra
import qualified Prelude as P
import Alt.Integer
import Alt.Maybe
import Alt.Logic
import Alt.TH.Num
import Language.Haskell.TH

mkIntInstance ''Int
mkIntInstance ''Int64
mkIntInstance ''Int32
mkIntInstance ''Int16
mkIntInstance ''Int8

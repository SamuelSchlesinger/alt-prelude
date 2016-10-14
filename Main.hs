{-# LANGUAGE PolyKinds, TypeOperators, TypeFamilies, DataKinds, RebindableSyntax #-}

module Main where

import Algebra

main = do
  print (4 :: Z `Mod` 3)

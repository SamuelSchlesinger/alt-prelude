{-# LANGUAGE PolyKinds, TypeOperators, TypeFamilies, DataKinds, RebindableSyntax #-}

module Main where

import Alt

main = do
  print (5 + 6 :: Integer `Mod` 4)

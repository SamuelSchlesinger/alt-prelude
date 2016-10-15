{-# LANGUAGE NoImplicitPrelude #-}

module Alt.IO
  ( 
    IO(..)
  , bindIO
  , returnIO
  , mapIO
  , print
  ) where

import System.IO (IO)
import Prelude (print)
import qualified Prelude as P

bindIO :: (a -> IO b) -> IO a -> IO b
bindIO = P.flip (P.>>=)

returnIO :: a -> IO a
returnIO = P.return

mapIO :: (a -> b) -> IO a -> IO b
mapIO = P.fmap

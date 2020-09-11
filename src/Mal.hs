{-# LANGUAGE TypeOperators #-}

-- |
--
-- Export the final type class of the simple lambda calculus language.
-- Here we finish the Lambda type class off with some basic operations on
-- integers.
module Mal (Mal (..)) where

import Data.Word (Word64)
import Mal.HasCoexp
import Mal.HasProduct
import Mal.HasSum
import Mal.Type

class (HasProduct k, HasCoexp k) => Mal k where
  u64 :: Word64 -> k Unit U64
  add :: k env U64 -> k env U64 -> k env U64

{-# LANGUAGE TypeOperators #-}

-- |
--
-- Export the final type class of the simple lambda calculus language.
-- Here we finish the Lambda type class off with some basic operations on
-- integers.
module Lambda (Lambda (..)) where

import Data.Word (Word64)
import Lambda.HasExp
import Lambda.HasProduct
import Lambda.HasSum
import Lambda.Type

class (HasSum k, HasProduct k, HasExp k) => Lambda k where
  u64 :: Word64 -> k Unit U64
  add :: k Unit (U64 ~> U64 ~> U64)

{-# LANGUAGE TypeOperators #-}

-- |
--
-- Export the final type class of the simple lambda calculus language.
-- Here we finish the Lambda type class off with some basic operations on
-- integers.
module Mal (Mal (..)) where

import Data.Word (Word64)
import Mal.HasCoexp
import Mal.HasSum
import Mal.Type

class HasCoexp k => Mal k where
  unit :: k Unit r
  u64 :: Word64 -> k U64 Void

-- add :: k Unit (U64 ~> U64 ~> U64)

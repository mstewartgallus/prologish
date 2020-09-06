{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Fn (Fn (..)) where

import Data.Word (Word64)
import Term.Type

class Fn t where
  head :: t (a ': env) a
  tail :: t env a -> t (any ': env) a

  curry :: t (a ': env) b -> t env (a ~> b)
  (<*>) :: t env (a ~> b) -> t env a -> t env b

  u64 :: Word64 -> t env U64
  add :: t env (U64 ~> U64 ~> U64)

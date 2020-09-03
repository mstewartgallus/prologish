{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Fn (Fn (..)) where

import Data.Word (Word64)
import Lambda.Type

class Fn t where
  (&&&) :: t a -> t b -> t (a * b)
  first :: t (a * b) -> t a
  second :: t (a * b) -> t b
  unit :: t Unit

  curry :: (t (b * a) -> t c) -> t a -> t (b ~> c)
  uncurry :: (t a -> t (b ~> c)) -> t (b * a) -> t c

  (<*>) :: t (a ~> b) -> t a -> t b

  u64 :: Word64 -> t U64
  add :: t (U64 ~> U64 ~> U64)

infixl 9 &&&

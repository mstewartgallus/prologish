{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas (Hoas (..)) where

import Data.Word (Word64)
import Hoas.Type
import Prelude hiding (uncurry, (.), (<*>))

class Hoas t where
  kont :: ST a -> t x -> (t a -> t Void) -> t (x |- a)

  -- do something like.. instead and remove val
  -- we need something like this for linearity.
  -- jump :: t (x |- a) -> (t x -> t a) -> t Void
  jump :: t (x |- a) -> t a -> t Void
  val :: t (x |- a) -> t x

  try :: (t (a -< b) -> t env) -> t b -> t (a + env)

  unit :: t Unit
  (&&&) :: t a -> t b -> t (a * b)
  first :: t (a * b) -> t a
  second :: t (a * b) -> t b

  absurd :: t Void -> t a
  either :: t (a + b) -> (t a -> t c, t b -> t c) -> t c
  left :: t a -> t (a + b)
  right :: t b -> t (a + b)

  pick :: t B -> t (Unit + Unit)
  true :: t B
  false :: t B

  u64 :: Word64 -> t U64
  add :: t U64 -> t U64 -> t U64

infixl 9 &&&

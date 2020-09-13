{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas.Bound (Bound (..)) where

import Hoas.Type
import Prelude hiding ((.), id, (<*>), uncurry)
import Id (Id)
import Data.Word (Word64)

class Bound t where
  kont :: Id -> ST a -> t x -> (t a -> t Void) -> t (x |- a)

  jump :: t (x |- a) -> t a -> t Void
  val :: t  (x |- a) -> t x

  unit :: t Unit
  (&&&) :: t a -> t b -> t (a * b)
  first :: t (a * b) -> t a
  second :: t (a * b) -> t b

  absurd :: t Void -> t a
  -- isEither :: t (a + b) -> (t (a -< c), t (b -< c)) -> t c
  left :: t a -> t (a + b)
  right :: t b -> t (a + b)

  pick :: t B -> t (Unit + Unit)
  true :: t B
  false :: t B

  u64 :: Word64 -> t U64
  add :: t U64 -> t U64 -> t U64

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas.Bound (Bound (..)) where

import Hoas.Type
import Prelude hiding ((.), id, (<*>), uncurry)
import Id (Id)
import Data.Word (Word64)

class Bound t where
  be :: Id -> t a -> ST a -> (t a -> t b) -> t b

  isUnit :: t Unit -> t x
  isBoth :: t (a * b) -> (t (a -< c), t (b -< c)) -> t c
  isFirst :: t a -> t (a * b)
  isSecond :: t b -> t (a * b)

  mal :: Id -> ST a -> (t a -> t b) -> t (a -< b)
  try :: t (a -< b) -> t a -> t b

  isAbsurd :: t Void
  (|||) :: t a -> t b -> t (a + b)
  isLeft :: t (a + b) -> t a
  isRight :: t (a + b) -> t b

  pick :: t (Unit + Unit) -> t B

  isU64 :: t U64 -> Word64 -> t Unit
  add :: t (U64 -< (U64 * U64))

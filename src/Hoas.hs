{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas (Hoas (..), letBe) where

import Data.Word (Word64)
import Hoas.Type
import Prelude hiding (id, uncurry, (.), (<*>))

class Hoas t where
  mal :: ST a -> (t a -> t b) -> t (a -< b)
  try :: t (a -< b) -> t a -> t b

  isAbsurd :: t Void
  isBoth :: t (a * b) -> (t (a -< c), t (b -< c)) -> t c
  isFirst :: t a -> t (a * b)
  isSecond :: t b -> t (a * b)

  isUnit :: t Unit -> t x
  (|||) :: t a -> t b -> t (a + b)
  isLeft :: t (a + b) -> t a
  isRight :: t (a + b) -> t b

  pick :: t (Unit + Unit) -> t B
  isTrue :: t B -> t Unit
  isFalse :: t B -> t Unit

  be :: t a -> ST a -> (t a -> t b) -> t b
  be x t f = mal t f `try` x

  isU64 :: t U64 -> Word64 -> t Unit
  add :: t (U64 -< (U64 * U64))

infixl 9 |||

letBe :: (KnownT a, Hoas t) => t a -> (t a -> t b) -> t b
letBe x f = be x inferT f

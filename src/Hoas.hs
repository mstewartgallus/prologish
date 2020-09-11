{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas (Hoas (..)) where

import Data.Word (Word64)
import Hoas.Type
import Prelude hiding (id, uncurry, (.), (<*>))

class Hoas t where
  mal :: ST b -> t a -> (t b -> t Void) -> t (a |- b)
  assume :: t (a -< b) -> t b
  deny :: t (a -< b) -> t a -> t Void

  unit :: t Unit
  (&&&) :: t a -> t b -> t (a * b)
  first :: t (a * b) -> t a
  second :: t (a * b) -> t b

  absurd :: t Void -> t a
  isEither :: t (a + b) -> (t (a -< c), t (b -< c)) -> t c
  left :: t a -> t (a + b)
  right :: t b -> t (a + b)

  pick :: t B -> t (Unit + Unit)
  true :: t B
  false :: t B

  isU64 :: t U64 -> Word64 -> t Void

infixl 9 &&&

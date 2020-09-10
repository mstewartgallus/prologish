{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas (Hoas (..), letBe) where

import Data.Word (Word64)
import Hoas.Type
import Prelude hiding (id, uncurry, (.), (<*>))

class Hoas t where
  done :: t Void

  mal :: ST a -> (t a -> t b) -> t (a -< b)
  try :: t (a -< b) -> t a -> t b

  -- U64 -< Unit => k (U64 -< Unit) Void => k Unit (U64 + Void) => k Unit U64
  u64 :: Word64 -> t (U64 -< Unit)
  add :: t (U64 -< U64 -< U64)

  pair :: t (a * b) -> t (a -< c) -> t (b -< c) -> t c
  first :: t a -> t (a * b)
  second :: t b -> t (a * b)

  be :: t a -> ST a -> (t a -> t b) -> t b
  be x t f = mal t f `try` x

letBe :: (KnownT a, Hoas t) => t a -> (t a -> t b) -> t b
letBe x f = be x inferT f

{-# LANGUAGE TypeOperators #-}

module Hoas (Hoas (..), letBe) where

import Data.Word (Word64)
import Hoas.Type
import Prelude hiding (id, uncurry, (.), (<*>))

class Hoas t where
  kont :: ST a -> (t a -> t b) -> t (a -< b)
  try :: t (a -< b) -> t a -> t b

  u64 :: Word64 -> t U64 -> t r
  add :: t (U64 -< U64 -< U64)

  be :: t a -> ST a -> (t a -> t b) -> t b
  be x t f = kont t f `try` x

letBe :: (KnownT a, Hoas t) => t a -> (t a -> t b) -> t b
letBe x f = be x inferT f

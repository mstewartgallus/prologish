{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas (Hoas (..), letBe) where

import Data.Word (Word64)
import qualified HasApply
import qualified HasWord
import Hoas.Type

class (HasApply.HasApply t, HasWord.HasWord t) => Hoas t where
  be :: t a -> ST a -> (t a -> t b) -> t b

  lam :: ST a -> (t a -> t b) -> t (a ~> b)

  add :: t (U64 ~> U64 ~> U64)

letBe :: (KnownT a, Hoas t) => t a -> (t a -> t b) -> t b
letBe x f = be x inferT f

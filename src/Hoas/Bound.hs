{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas.Bound (Bound (..)) where

import Hoas.Type
import Id (Id)
import HasWord (HasWord)
import HasApply (HasApply)
import Data.Word (Word64)

class (HasApply t, HasWord t) => Bound t where
  be :: Id -> t a -> ST a -> (t a -> t b) -> t b

  lam :: Id -> ST a -> (t a -> t b) -> t (a ~> b)

  add :: t (U64 ~> U64 ~> U64)

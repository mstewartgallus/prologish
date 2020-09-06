{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Term (Term (..), letBe) where

import Data.Word (Word64)
import Term.Type
import Prelude hiding (id, uncurry, (.), (<*>))

class Term t where
  lam :: ST a -> (t a -> t b) -> t (a ~> b)
  (<*>) :: t (a ~> b) -> t a -> t b

  u64 :: Word64 -> t U64
  add :: t (U64 ~> U64 ~> U64)

letBe :: (KnownT a, Term t) => t a -> (t a -> t b) -> t b
letBe x f = lam inferT f <*> x

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Term.Bound (Bound (..)) where

import Term.Type
import Prelude hiding ((.), id, (<*>), uncurry)
import Id (Id)
import Data.Word (Word64)

class Bound t where
  be :: Id -> t a -> ST a -> (t a -> t b) -> t b

  lam :: Id -> ST a -> (t a -> t b) -> t (a ~> b)
  (<*>) :: t (a ~> b) -> t a -> t b

  u64 :: Word64 -> t U64
  add :: t (U64 ~> U64 ~> U64)

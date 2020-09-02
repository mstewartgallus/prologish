{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Lambda.Bound (Bound (..)) where

import Control.Category
import Lambda.Type
import Prelude hiding ((.), id, (<*>), uncurry)
import Id (Id)
import Data.Word (Word64)

class Bound t where
  lam :: Id -> ST a -> (t a -> t b) -> t (a ~> b)
  (<*>) :: t (a ~> b) -> t a -> t b

  u64 :: Word64 -> t U64
  add :: t (U64 ~> U64 ~> U64)

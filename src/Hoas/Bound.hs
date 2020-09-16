{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas.Bound (Bound (..)) where

import Hoas.Type
import Prelude hiding ((.), id, (<*>), uncurry)
import Id (Id)
import Hoas.Global
import Data.Kind
import Data.Word (Word64)
import Control.Category

class Category t => Bound t where
  label :: Id -> ST c -> (t c Void -> t b a) -> t b (c + a)
  lift :: t c Void -> t (c + a) a

  mal :: Id -> ST c -> (t c Void -> t b a) -> t (c -< b) a
  pass :: t c Void -> t b (c -< b)

  global :: Global a b -> t a b

  u64 :: Word64 -> t x U64

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Mal.HasCoexp (HasCoexp (..)) where

import Control.Category
import Mal.HasSum
import Mal.Type
import Prelude hiding ((.), id, (<*>), uncurry)

-- | The categorical definition of a coexponential
class HasSum k => HasCoexp k where
  mal :: k b (a + env) -> k (a -< b) env
  try :: k (a -< b) env -> k b (a + env)

  kont :: k x b -> k c Void -> k x (b |- c)
  kont x f = (((absurd . f) ||| id) . try id) . x

  val :: k (b |- a) b
  val = mal right

  jump :: k x (b |- a) -> k b a -> k x Void
  jump f x = mal (left . x) . f

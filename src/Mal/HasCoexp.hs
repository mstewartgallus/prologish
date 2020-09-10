{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Mal.HasCoexp (HasCoexp (..)) where

import Control.Category
import Mal.HasSum
import Mal.Type
import Prelude hiding ((.), id, (<*>), uncurry)

-- | The categorical definition of a coexponential
class HasSum k => HasCoexp k where
  tryCatch :: k (a -< b) env -> k a env -> k b env
  f `tryCatch` x = (x ||| id) . try f

  mal :: k b (a + env) -> k (a -< b) env
  try :: k (a -< b) env -> k b (a + env)

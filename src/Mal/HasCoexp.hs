{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Mal.HasCoexp (HasCoexp (..)) where

import Control.Category
import Mal.HasSum
import Mal.Type
import Prelude hiding ((.), id, (<*>), curry, uncurry)

-- | The categorical definition of an exponential (function type.)
class HasSum k => HasCoexp k where
  (<*>) :: k (a -< b) env -> k a env -> k b env
  f <*> x =  (x ||| id) . try f

  mal :: k b (a + env) -> k (a -< b) env
  try :: k (a -< b) env -> k b (a + env)

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Lambda.HasExp (HasExp (..)) where

import Control.Category
import Lambda.HasProduct
import Lambda.Type
import Prelude hiding ((.), id, (<*>), uncurry)

-- | The categorical definition of an exponential (function type.)
class HasProduct k => HasExp k where
  (<*>) :: k env (a ~> b) -> k env a -> k env b
  f <*> x = uncurry f . (x &&& id)

  curry :: k (a * env) b -> k env (a ~> b)
  uncurry :: k env (a ~> b) -> k (a * env) b

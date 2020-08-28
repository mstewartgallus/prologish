{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Lambda.Exp (Exp (..)) where

import Control.Category
import Lambda.Product
import Lambda.Type
import Prelude hiding ((.), id, (<*>), uncurry)

-- | The categorical definition of an exponential (function type.)
class Product k => Exp k where
  (<*>) :: k env (a ~> b) -> k env a -> k env b
  f <*> x = uncurry f . (id &&& x)

  curry :: k (env * a) b -> k env (a ~> b)
  uncurry :: k env (a ~> b) -> k (env * a) b

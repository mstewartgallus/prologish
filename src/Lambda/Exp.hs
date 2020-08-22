{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Lambda.Exp (Exp (..)) where

import Control.Category
import Lambda.Product
import Lambda.Type
import Prelude hiding ((.), id)

class Product k => Exp k where
  lambda :: k (env * a) b -> k env (a ~> b)
  eval :: k env (a ~> b) -> k (env * a) b

  (<*>) :: k env (a ~> b) -> k env a -> k env b
  f <*> x = eval id <<< (f # x)

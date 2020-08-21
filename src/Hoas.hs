{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas (Hoas (..)) where

import Control.Category
import Product
import Sum
import Type
import Prelude hiding ((.), id)

class (Sum k, Product k) => Hoas k where
  lift :: ST a -> (k Unit a -> k Unit b) -> k a b

  var :: ST env -> ST a -> (k Unit a -> k env b) -> k (env * a) b
  var env t f = lift (env :*: t) $ \x -> f (second . x) . first . x

  label :: ST a -> (Continuation k a -> k b env) -> k b (env + a)

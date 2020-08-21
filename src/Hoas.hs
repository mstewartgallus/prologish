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
  mapVar :: ST a -> (k Unit a -> k Unit b) -> k a b

  var :: ST env -> ST a -> (k Unit a -> k env b) -> k (env * a) b
  var env t f = mapVar (env :*: t) $ \x -> f (second . x) . first . x

  mapLabel :: ST b -> (k b Void -> k a Void) -> k a b

  label :: ST env -> ST b -> (k b Void -> k a env) -> k a (env + b)
  label env t f = mapLabel (env :+: t) $ \x -> x . left . f (x . right)

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Lambda.Hoas (Hoas (..), label, var) where

import Control.Category
import Lambda.Product
import Lambda.Sum
import Lambda.Type
import Prelude hiding ((.), id)

class Category k => Hoas k where
  mapVar :: ST a -> (k Unit a -> k Unit b) -> k a b
  mapLabel :: ST b -> (k b Void -> k a Void) -> k a b

var :: (Hoas k, Product k) => ST env -> ST a -> (k Unit a -> k env b) -> k (env * a) b
var env t f = mapVar (env :*: t) $ \x -> f (second . x) . first . x

label :: (Hoas k, Sum k) => ST env -> ST b -> (k b Void -> k a env) -> k a (env + b)
label env t f = mapLabel (env :+: t) $ \x -> x . left . f (x . right)

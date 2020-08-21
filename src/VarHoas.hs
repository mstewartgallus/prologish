{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module VarHoas (VarHoas (..), var) where

import Control.Category
import Product
import Type
import Prelude hiding ((.), id)

class Category k => VarHoas k where
  mapVar :: ST a -> (k Unit a -> k Unit b) -> k a b

var :: (VarHoas k, Product k) => ST env -> ST a -> (k Unit a -> k env b) -> k (env * a) b
var env t f = mapVar (env :*: t) $ \x -> f (second . x) . first . x

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoStarIsType #-}

module Lambda.Hoas (Hoas (..), letBe, letCase) where

import Control.Category
import Lambda.Product
import Lambda.Sum
import Lambda.Type
import Prelude hiding ((.), id)

class Category k => Hoas k where
  implicitLabel :: ST b -> (k b Void -> k a Void) -> k b env -> k a env
  implicitEnv :: ST a -> (k Unit a -> k Unit b) -> (k env a -> k env b)

-- | fixme... figure out how to derivae labeling from the yoneda embedding ...
yoneda :: Category k => (forall x. k x a -> k x b) -> k a b
yoneda f = f id

letBe :: (KnownT a, KnownT env, Hoas k, Product k) => k env a -> (k Unit a -> k env b) -> k env b
letBe x f = implicitEnv inferT (\x -> f (second . x) . first . x) (id # x)

infixr 0 `letBe`

letCase :: (KnownT b, KnownT env, Hoas k, Sum k) => k b env -> (k b Void -> k a env) -> k a env
letCase x f = implicitLabel inferT (\x -> x . left . f (x . right)) (id ! x)

infixr 0 `letCase`

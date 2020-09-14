{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Mal.HasCoexp (HasCoexp (..)) where

import Control.Category
import Mal.HasSum
import Mal.Type
import Prelude hiding ((.), id, (<*>), uncurry)

-- | The categorical definition of a coexponential
class HasSum k => HasCoexp k where
  -- | dual to curry
  mal :: k b (a + env) -> k (a -< b) env
  -- | dual to uncurry
  try :: k (a -< b) env -> k b (a + env)

  -- | kont rule
  --
  --- env |- v : b      (x : c) |- y : Void
  -- ---------------------------------
  --  env |- v kont (x : c). y : c -< b
  kont :: k env b -> k c Void -> k env (b |- c)
  kont x f = (((absurd . f) ||| id) . try id) . x

  -- | val rule, probably will only be useful for debugging purposes,
  -- mostly.
  --
  --- env |- k : a -< b
  -- ---------------------------------
  --  env |- val k : b
  val :: k env (b |- a) -> k env b
  val x = mal right . x

  -- | jump rule
  --
  --- env |- k : a -< b      b |- x : a
  -- ---------------------------------
  --  env |- k x
  jump :: k env (b |- a) -> k b a -> k env Void
  jump k x = mal (left . x) . k

  coid :: k (a -< a) Void
  coid = mal left

  coapply :: k (a -< b) env -> k a env -> k b env
  f `coapply` x = (x ||| id) . try f

  compose :: k (b -< c) env -> k (a -< b) env -> k (a -< c) env
  compose f g = mal ((g' ||| right) . f') where
    f' = try f
    g' = try g

  annihilate :: k env (a |- a) -> k env Void
  annihilate x = coid . x

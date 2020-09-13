{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Mal.HasCoexp (HasCoexp (..)) where

import Control.Category
import Mal.HasSum
import Mal.Type
import Prelude hiding ((.), id, (<*>), uncurry)

-- | The categorical definition of a coexponential
class HasSum k => HasCoexp k where
  mal :: k b (a + env) -> k (a -< b) env
  try :: k (a -< b) env -> k b (a + env)

  -- | kont rule
  --
  --- env |- v : b      (x : c) |- y : Void
  -- ---------------------------------
  --  env |- v kont (x : c). y : c -< b
  kont :: k env b -> k c Void -> k env (b |- c)
  kont x f = (((absurd . f) ||| id) . try id) . x

  -- | val rule, gives the closure environment !
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

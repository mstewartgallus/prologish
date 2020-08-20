{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Cbpv (Cbpv (..)) where

import Data.Kind
import Data.Word (Word64)
import Sort
import Prelude hiding ((.), id)

class Cbpv (k :: forall x y. x -> y -> Type) where
  id :: k a a
  (.) :: k b c -> k a b -> k a c

  absurd :: k Void x

  (!) :: k a c -> k b c -> k (a + b) c
  left :: k a (a + b)
  right :: k b (a + b)

  unit :: k x Unit
  (#) :: k env a -> k env b -> k env (a * b)
  first :: k (a * b) a
  second :: k (a * b) b

  lambda :: k (env * a) b -> k env (a ~> b)
  unlambda :: k env (a ~> b) -> k (env * a) b

  force :: k (U a) a
  thunk :: k a (U a)
  returns :: k a (F a)

  u64 :: Word64 -> k x U64
  add :: k x (U64 ~> U64 ~> F U64)

infixl 9 #

infixl 9 !

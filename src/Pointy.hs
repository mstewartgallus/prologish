{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Pointy (Pointy (..)) where

import Data.Word (Word64)
import Lambda.Type
import Prelude hiding (uncurry)

class Pointy t where
  u64 :: Word64 -> t Unit -> t U64
  add :: t Unit -> t (U64 ~> U64 ~> U64)

  curry :: (t (a * env) -> t b) -> (t env -> t (a ~> b))
  uncurry :: (t env -> t (a ~> b)) -> (t (a * env) -> t b)

  (<*>) :: (t env -> t (a ~> b)) -> (t env -> t a) -> t env -> t b
  f <*> x = uncurry f . (x &&& id)

  unit :: t x -> t Unit

  (&&&) :: (t env -> t a) -> (t env -> t b) -> (t env -> t (a * b))
  first :: t (a * b) -> t a
  second :: t (a * b) -> t b

  absurd :: t Void -> t x

  (|||) :: (t a -> t c) -> (t b -> t c) -> (t (a + b) -> t c)
  left :: t a -> t (a + b)
  right :: t b -> t (a + b)

infixl 9 |||

infixl 9 &&&

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Term (Term (..)) where

import Data.Word (Word64)
import Hoas.Type
import Prelude hiding (const, curry, (<*>))

class Term t where
  mal :: t (b : x) (a + env) -> t x (b |- a) -> t x env
  try :: t (b |- a : x) env -> t (a : x) env -> t x b -> t x env

  tip :: t (a ': env) a
  const :: t env a -> t (any ': env) a
  swap :: t (x ': a ': env) b -> t (a ': x ': env) b

  unit :: t env Unit
  (&&&) :: t env a -> t env b -> t env (a * b)
  first :: t env (a * b) -> t env a
  second :: t env (a * b) -> t env b

  absurd :: t env Void -> t env a

  -- isEither :: t (a + b) -> (t (a -< c), t (b -< c)) -> t c
  left :: t env a -> t env (a + b)
  right :: t env b -> t env (a + b)

  u64 :: Word64 -> t env U64
  add :: t env U64 -> t env U64 -> t env U64

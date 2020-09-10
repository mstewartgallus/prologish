{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Term (Term (..)) where

import Data.Word (Word64)
import Hoas.Type
import Prelude hiding (const, curry, (<*>))

class Term t where
  be :: t a env -> t b (a ': env) -> t b env

  tip :: t a (a ': env)
  const :: t a env -> t a (any ': env)

  throw :: t b (a ': env) -> t (a -< b) env
  try :: t (a -< b) env -> t a env -> t b env

  u64 :: Word64 -> t U64 env
  add :: t (U64 -< U64 -< U64) env

  swap :: t b (x ': a ': env) -> t b (a ': x ': env)
  swap f = const (const (throw (throw f))) `try` tip `try` const tip

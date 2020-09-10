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

  mal :: t b (a ': env) -> t (a -< b) env
  try :: t (a -< b) env -> t a env -> t b env

  done :: t Void r

  u64 :: Word64 -> t U64 r -> t x r
  add :: t (U64 -< U64 -< U64) env

  swap :: t b (x ': a ': env) -> t b (a ': x ': env)
  swap f = const (const (mal (mal f))) `try` tip `try` const tip

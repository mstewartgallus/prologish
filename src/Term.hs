{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Term (Term (..)) where

import Data.Word (Word64)
import Hoas.Type
import Prelude hiding (const, curry, (<*>))

class Term t where
  tip :: t a (a ': env)
  const :: t a env -> t a (any ': env)

  mal :: t b (a ': env) -> t (a -< b) env
  try :: t (a -< b) env -> t a env -> t b env

  isUnit :: t Unit r -> t a r
  isBoth :: t (a * b) r -> (t (a -< c) r, t (b -< c) r) -> t c r
  isFirst :: t a r -> t (a * b) r
  isSecond :: t b r -> t (a * b) r

  absurd :: t Void r
  (|||) :: t a c -> t b c -> t (a + b) c
  isLeft :: t (a + b) r -> t a r
  isRight :: t (a + b) r -> t b r

  isU64 :: t U64 r -> Word64 -> t Unit r
  add :: t (U64 -< (U64 * U64)) r

  swap :: t b (x ': a ': env) -> t b (a ': x ': env)
  swap f = const (const (mal (mal f))) `try` tip `try` const tip

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Term (Term (..)) where

import Data.Word (Word64)
import Hoas.Type
import Prelude hiding (const, curry, (<*>))

class Term t where
  be :: t env a -> t (a ': env) b -> t env b

  tip :: t (a ': env) a
  const :: t env a -> t (any ': env) a

  curry :: t (a ': env) b -> t env (a ~> b)
  (<*>) :: t env (a ~> b) -> t env a -> t env b

  u64 :: Word64 -> t env U64
  add :: t env (U64 ~> U64 ~> U64)

  swap :: t (x ': a ': env) b -> t (a ': x ': env) b
  swap f = const (const (curry (curry f))) <*> tip <*> const tip

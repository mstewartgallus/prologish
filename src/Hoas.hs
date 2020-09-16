{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Hoas (Hoas (..)) where

import Data.Kind
import Data.Word (Word64)
import Hoas.Type
import Prelude hiding (break, (<*>))

class Hoas t where
  data Jump t
  data Expr t :: T -> Type
  data Case t :: T -> Type

  mal :: ST a -> ST b -> (Case t a -> Case t b) -> Case t (a -< b)
  mal a b f = adbmal a b (\x -> jump (f x))

  adbmal :: ST a -> ST b -> (Case t a -> Expr t b -> Jump t) -> Case t (a -< b)
  try :: Case t (a -< b) -> Case t a -> Case t b

  unit :: Expr t Unit
  (&&&) :: Expr t a -> Expr t b -> Expr t (a * b)
  first :: Expr t (a * b) -> Expr t a
  second :: Expr t (a * b) -> Expr t b

  empty :: Case t Void
  (|||) :: Case t a -> Case t b -> Case t (a + b)
  left :: Case t (a + b) -> Case t a
  right :: Case t (a + b) -> Case t b

  jump :: Case t a -> Expr t a -> Jump t
  thunk :: ST a -> (Case t a -> Jump t) -> Expr t a
  letBe :: ST a -> (Expr t a -> Jump t) -> Case t a

  kont ::
    ST a ->
    ST b ->
    Expr t a ->
    (Expr t b -> Jump t) ->
    Expr t (b -< a)
  kont s t x f = thunk (t :-< s) (\k -> (k `try` letBe t f) `jump` x)

  jmp :: ST a -> ST b -> Expr t (a -< b) -> Expr t a -> Jump t
  jmp t b k x = adbmal t b (\k _ -> jump k x) `jump` k

  env :: ST a -> ST b -> Expr t (a -< b) -> Expr t b
  env a b k = thunk b $ \x -> mal a b (const x) `jump` k

  u64 :: Word64 -> Expr t U64

  (|=) ::
    (KnownT a, KnownT b) =>
    Expr t a ->
    (Expr t b -> Jump t) ->
    Expr t (b -< a)
  x |= f = kont inferT inferT x f

  (!) ::
    (KnownT a, KnownT b) =>
    Expr t (a -< b) ->
    Expr t a ->
    Jump t
  k ! x = jmp inferT inferT k x

infixl 0 |=

infixr 9 &&&

infixr 9 |||

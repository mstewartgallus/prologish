{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Hoas.AsBound (Expr, bindPoints) where

import Id (Stream (..))
import Hoas.Bound
import qualified Hoas
import Hoas.Type
import Prelude hiding ((.), id, curry, uncurry, (<*>))

newtype Expr t (a :: T) = E (Stream -> t a)

bindPoints :: Stream -> Expr t a -> t a
bindPoints str (E x) = x str

instance Bound t => Hoas.Hoas (Expr t) where
  kont t (E x) k = E $ \(Stream n xs ys) -> kont n t (x xs) $ \x -> case k (E $ \_ -> x) of
    E y -> y ys

  jump t (E f) k = E $ \(Stream n fs ys) -> jump n t (f fs) $ \x -> case k (E $ \_ -> x) of
                        E y -> y ys
  val = lift1 val

  unit = lift0 unit
  (&&&) = lift2 (&&&)
  first = lift1 first
  second = lift1 second

  absurd = lift1 absurd
  -- E x `isEither` (E f, E g) = E $ \(Stream _ xs (Stream _ gs fs)) -> x xs `isEither` (f fs, g gs)
  left = lift1 left
  right = lift1 right

  pick = lift1 pick
  true = lift0 true
  false = lift0 false

  u64 n = lift0 (u64 n)
  add = lift2 add

lift0 :: t a -> Expr t a
lift0 x = E $ const x

lift1 :: (t a -> t b) -> Expr t a -> Expr t b
lift1 f (E x) = E $ \xs -> f (x xs)

lift2 :: (t a -> t b -> t c) -> Expr t a -> Expr t b -> Expr t c
lift2 f (E x) (E y) = E $ \(Stream _ xs ys) -> f (x xs) (y ys)

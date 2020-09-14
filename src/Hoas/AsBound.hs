{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hoas.AsBound (Expr, bindPoints) where

import Id (Stream (..))
import Hoas.Bound
import qualified Hoas
import Hoas.Type
import Data.Kind
import Prelude hiding ((.), id, curry, uncurry, (<*>))

newtype Expr p (n :: T -> Type) (a :: T) = E (Stream -> p a)
newtype Neg (p :: T -> Type) n (a :: T) = N (Stream -> n a)

bindPoints :: Stream -> Expr p n a -> p a
bindPoints str (E x) = x str

instance Bound pos neg => Hoas.Hoas (Expr pos neg) (Neg pos neg) where
  kont t (E x) k = E $ \(Stream n xs ys) -> kont n t (x xs) $ \x -> case k (E $ \_ -> x) of
    E y -> y ys

  jump t (E f) k = E $ \(Stream n fs ys) -> jump n t (f fs) $ \x -> case k (E $ \_ -> x) of
                        E y -> y ys
  val = lift1 val

  unit = lift0 unit
  (&&&) = lift2 (&&&)
  first = lift1 first
  second = lift1 second

  absurd = lift0' absurd
  (|||) = lift2' (|||)
  left = lift1' left
  right = lift1' right

  pick = lift1 pick
  true = lift0 true
  false = lift0 false

  u64 n = lift0 (u64 n)
  add = lift2 add

  load t name = lift0 $ load t name

lift0 :: t a -> Expr t n a
lift0 x = E $ const x

lift1 :: (t a -> t b) -> Expr t n a -> Expr t n b
lift1 f (E x) = E $ \xs -> f (x xs)

lift2 :: (t a -> t b -> t c) -> Expr t n a -> Expr t n b -> Expr t n c
lift2 f (E x) (E y) = E $ \(Stream _ xs ys) -> f (x xs) (y ys)

lift0' :: n a -> Neg t n a
lift0' x = N $ const x

lift1' :: (n a -> n b) -> Neg t n a -> Neg t n b
lift1' f (N x) = N $ \xs -> f (x xs)

lift2' :: (n a -> n b -> n c) -> Neg t n a -> Neg t n b -> Neg t n c
lift2' f (N x) (N y) = N $ \(Stream _ xs ys) -> f (x xs) (y ys)

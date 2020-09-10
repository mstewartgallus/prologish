{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Hoas.AsBound (Expr, bindPoints) where

import Id (Stream (..))
import Hoas.Bound
import qualified Hoas
import Hoas.Type
import Prelude hiding ((.), id, curry, uncurry, (<*>))

newtype Expr t (a :: T) = Expr (Stream -> t a)

bindPoints :: Stream -> Expr t a -> t a
bindPoints str (Expr x) = x str

instance Bound t => Hoas.Hoas (Expr t) where
  be (Expr x) t f = Expr $ \(Stream n xs ys) -> be n (x xs) t $ \x' -> case f (Expr $ \_ -> x') of
    Expr y -> y ys

  kont t f = Expr $ \(Stream n _ ys) -> kont n t $ \x -> case f (Expr $ \_ -> x) of
    Expr y -> y ys
  Expr f `try` Expr x = Expr $ \(Stream _ fs xs) -> f fs `try` x xs

  u64 x = Expr $ const (u64 x)
  add = Expr $ const add

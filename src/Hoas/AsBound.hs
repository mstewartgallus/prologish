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

  mal t f = Expr $ \(Stream n _ ys) -> mal n t $ \x -> case f (Expr $ \_ -> x) of
    Expr y -> y ys
  Expr f `try` Expr x = Expr $ \(Stream _ fs xs) -> f fs `try` x xs

  isUnit (Expr x) = Expr $ \s -> isUnit (x s)
  Expr x `isBoth` (Expr f, Expr g) = Expr $ \(Stream _ xs (Stream _ fs gs)) -> x xs `isBoth` (f fs, g gs)
  isFirst (Expr x) = Expr $ \s -> isFirst (x s)
  isSecond (Expr x) = Expr $ \s -> isSecond (x s)

  isAbsurd = Expr $ const isAbsurd
  isEither (Expr f) (Expr g) = Expr $ \(Stream _ fs gs) -> isEither (f fs) (g gs)
  isLeft (Expr x) = Expr $ \s -> isLeft (x s)
  isRight (Expr x) = Expr $ \s -> isRight (x s)

  Expr x `isU64` n = Expr $ \s -> x s `isU64` n
  add = Expr $ const add

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Hoas.AsBound (Expr, bindPoints) where

import Id (Stream (..))
import Hoas.Bound
import qualified Hoas
import Hoas.Type
import Prelude hiding ((.), id, curry, uncurry, (<*>), break)

data Expr t (a :: T) (b :: T) = E (Stream -> t a b)

bindPoints :: Stream -> Expr t a b -> t a b
bindPoints str (E x) = x str

instance Bound t => Hoas.Hoas (Expr t) where
  mal t f = E $ \(Stream n _ ys) -> mal n t $ \x -> case f (E $ \_ -> x) of
    E y -> y ys
  E f `try` E x = E $ \(Stream _ fs xs) -> f fs `try` x xs

  thunk t f = E $ \(Stream n _ ys) -> thunk n t $ \x -> case f (E $ \_ -> x) of
    E y -> y ys
  letBe t f = E $ \(Stream n _ ys) -> letBe n t $ \x -> case f (E $ \_ -> x) of
    E y -> y ys

  E f `jump` E x = E $ \(Stream _ fs xs) -> f fs `jump` x xs

  unit = E $ const unit
  E f ||| E x = E $ \(Stream _ fs xs) -> f fs ||| x xs

  empty = E $ const empty

  u64 x = E $ const (u64 x)

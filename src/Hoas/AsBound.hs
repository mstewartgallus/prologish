{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Hoas.AsBound (Expr, bindPoints) where

import Id (Stream (..))
import Hoas.Bound
import qualified Hoas
import HasSum
import HasProduct
import Type
import Control.Category
import Prelude hiding ((.), id, curry, uncurry, (<*>), break)

data Expr t (a :: T) (b :: T) = E (Stream -> t a b)

bindPoints :: Stream -> Expr t a b -> t a b
bindPoints str (E x) = x str

instance Category t => Category (Expr t) where
  id = E $ const id
  E f . E x = E $ \(Stream _ fs xs) -> f fs . x xs

instance Bound t => HasSum (Expr t) where
  absurd = E $ const absurd
  E f ||| E x = E $ \(Stream _ fs xs) -> f xs ||| x xs

instance Bound t => HasProduct (Expr t) where
  E f &&& E x = E $ \(Stream _ fs xs) -> f xs &&& x xs
  first = E $ const first
  second = E $ const second

instance Bound t => Hoas.Hoas (Expr t) where
  st t f = E $ \(Stream n _ ys) -> st n t $ \x -> case f (E $ \_ -> x) of
    E y -> y ys
  E x `constrain` E f = E $ \(Stream _ fs xs) -> f fs `try` x xs

  E x `amb` E y = E $ \(Stream _ xs ys) -> x xs `amb` y ys

  true = E $ const true
  false = E $ const false

  u64 x = E $ const (u64 x)

  global g = E $ const (global g)

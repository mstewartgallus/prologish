{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Hoas.AsBound (Expr, bindPoints) where

import Id (Stream (..))
import Hoas.Bound
import qualified Hoas
import Type
import Control.Category
import Prelude hiding ((.), id, curry, uncurry, (<*>), break)

data Expr t (a :: T) (b :: T) = E (Stream -> t a b)

bindPoints :: Stream -> Expr t a b -> t a b
bindPoints str (E x) = x str

instance Category t => Category (Expr t) where
  id = E $ const id
  E f . E x = E $ \(Stream _ fs xs) -> f fs . x xs

instance Cokappa t => Hoas.Cokappa (Expr t) where
  label t f = E $ \(Stream n _ ys) -> label n t $ \x -> case f (E $ \_ -> x) of
    E y -> y ys
  lift (E x) = E $ \xs -> lift (x xs)

instance Cozeta t => Hoas.Cozeta (Expr t) where
  mal t f = E $ \(Stream n _ ys) -> mal n t $ \x -> case f (E $ \_ -> x) of
    E y -> y ys
  pass (E x) = E $ \xs -> pass (x xs)

instance Bound t => Hoas.Hoas (Expr t) where
  u64 x = E $ const (u64 x)

  global g = E $ const (global g)

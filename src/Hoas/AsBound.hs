{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Hoas.AsBound (Expr, bindPoints) where

import Id (Stream (..))
import Hoas.Bound
import qualified Hoas
import Hoas.Type
import Control.Category
import Prelude hiding ((.), id, curry, uncurry, (<*>), break)

data Expr t (a :: T) (b :: T) = E (Stream -> t a b)

bindPoints :: Stream -> Expr t a b -> t a b
bindPoints str (E x) = x str

instance Category t => Category (Expr t) where
  id = E $ const id
  E f . E x = E $ \(Stream _ fs xs) -> f fs . x xs

instance Bound t => Hoas.Hoas (Expr t) where
  letLabel t f = E $ \(Stream n _ ys) -> letLabel n t $ \x -> case f (E $ \_ -> x) of
    E y -> y ys

  adbmal (E f) = E $ \fs -> mal (f fs)
  E f `try` E x = E $ \(Stream _ fs xs) -> f fs `try` x xs

  unit = E $ const unit
  E f ||| E x = E $ \(Stream _ fs xs) -> f fs ||| x xs

  empty = E $ const empty

  u64 x = E $ const (u64 x)

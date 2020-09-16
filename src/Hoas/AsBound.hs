{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Hoas.AsBound (Expr, bindPoints) where

import Id (Stream (..))
import Hoas.Bound
import qualified Hoas
import Hoas.Type
import Prelude hiding ((.), id, curry, uncurry, (<*>), break)

data AsBound t

bindPoints :: Stream -> Hoas.Expr (AsBound t) a -> Expr t a
bindPoints str (E x) = x str

instance Bound t => Hoas.Hoas (AsBound t) where
  newtype Jump (AsBound t) = J (Stream -> Jump t)
  newtype Expr (AsBound t) (a :: T) = E (Stream -> Expr t a)
  newtype Case (AsBound t) (a :: T) = C (Stream -> Case t a)

  adbmal s t f = C $ \(Stream m _ (Stream n _ ys)) -> adbmal m s n t $ \x y -> case f (C $ \_ -> x) (E $ \_ -> y) of
    J y -> y ys
  C f `try` C x = C $ \(Stream _ fs xs) -> f fs `try` x xs

  thunk t f = E $ \(Stream n _ ys) -> thunk n t $ \x -> case f (C $ \_ -> x) of
    J y -> y ys
  letBe t f = C $ \(Stream n _ ys) -> letBe n t $ \x -> case f (E $ \_ -> x) of
    J y -> y ys

  C f `jump` E x = J $ \(Stream _ fs xs) -> f fs `jump` x xs

  unit = E $ const unit
  C f ||| C x = C $ \(Stream _ fs xs) -> f fs ||| x xs

  empty = C $ const empty

  u64 x = E $ const (u64 x)

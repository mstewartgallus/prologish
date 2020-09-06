{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Term.AsBoundTerm (Expr, bindPoints) where

import Id (Stream (..))
import Term.Bound
import qualified Term
import Term.Type
import Prelude hiding ((.), id, curry, uncurry, (<*>))

newtype Expr t (a :: T) = Expr (Stream -> t a)

bindPoints :: Stream -> Expr t a -> t a
bindPoints str (Expr x) = x str

instance Bound t => Term.Term (Expr t) where
  be (Expr x) t f = Expr $ \(Stream n xs ys) -> be n (x xs) t $ \x' -> case f (Expr $ \_ -> x') of
    Expr y -> y ys

  lam t f = Expr $ \(Stream n _ ys) -> lam n t $ \x -> case f (Expr $ \_ -> x) of
    Expr y -> y ys
  Expr f <*> Expr x = Expr $ \(Stream _ fs xs) -> f fs <*> x xs

  u64 x = Expr $ const (u64 x)
  add = Expr $ const add

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
  mal t (E x) k = E $ \(Stream n xs ys) -> mal n t (x xs) $ \x -> case k (E $ \_ -> x) of
    E y -> y ys

  assume (E x) = E $ \s -> assume (x s)
  deny (E f) (E x) = E $ \(Stream _ fs xs) -> f fs `deny` x xs

  unit = E $ const unit
  E f &&& E g = E $ \(Stream _ fs gs) -> f fs &&& g gs
  first (E x) = E $ \s -> first (x s)
  second (E x) = E $ \s -> second (x s)

  absurd (E x) = E $ \s -> absurd (x s)
  E x `isEither` (E f, E g) = E $ \(Stream _ xs (Stream _ gs fs)) -> x xs `isEither` (f fs, g gs)
  left (E x) = E $ \s -> left (x s)
  right (E x) = E $ \s -> right (x s)

  pick (E f) = E $ \s -> pick (f s)

  E x `isU64` n = E $ \s -> x s `isU64` n

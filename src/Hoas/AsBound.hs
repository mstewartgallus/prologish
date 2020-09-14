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
  kont t (E x) k = E $ \(Stream n xs ys) -> kont n t (x xs) $ \x -> case k (E $ \_ -> x) of
    E y -> y ys

  jump t (E f) k = E $ \(Stream n fs ys) -> jump n t (f fs) $ \x -> case k (E $ \_ -> x) of
                        E y -> y ys
  val (E x) = E $ \s -> val (x s)

  unit = E $ const unit
  E f &&& E g = E $ \(Stream _ fs gs) -> f fs &&& g gs
  first (E x) = E $ \s -> first (x s)
  second (E x) = E $ \s -> second (x s)

  absurd (E x) = E $ \s -> absurd (x s)
  -- E x `isEither` (E f, E g) = E $ \(Stream _ xs (Stream _ gs fs)) -> x xs `isEither` (f fs, g gs)
  left (E x) = E $ \s -> left (x s)
  right (E x) = E $ \s -> right (x s)

  pick (E f) = E $ \s -> pick (f s)

  u64 n = E $ const $ u64 n
  add (E x) (E y) = E $ \(Stream _ xs ys) -> add (x xs) (y ys)

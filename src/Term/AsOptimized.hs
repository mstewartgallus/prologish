{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Term.AsOptimized (Value, optimize) where

import Control.Category
import Term
import Data.Kind
import Hoas.Type
import Data.Word
import Prelude hiding ((.), (<*>), id, curry, uncurry, Either (..))

-- | The optimizer is based off the idea of normalization by
-- evaluation. Next this should really be moved back to the term level
-- representation
optimize :: Term t => Expr t '[] a -> t '[] a
optimize x = compile x

data Env (t :: [T] -> T -> Type) (a :: [T]) where
  Empty :: Env t '[]
  Push :: Value t h -> Env t n -> Env t (h ': n)

newtype Expr (t :: [T] -> T -> Type) env a = Expr (Env t env -> Value t a)

data Value (t :: [T] -> T -> Type) a where
  Embed :: (forall env. t env a) -> Value t a
  Val64 :: Word64 -> Value t U64
  Add :: Value t (U64 ~> U64 ~> U64)
  Curry :: (Env t (a : c) -> Value t b) -> Env t c -> Value t (a ~> b)

toTerm :: Term t => Value t a -> t '[] a
toTerm expr = case expr of
  Embed x -> x
  Val64 x -> u64 x
  Add -> add
  Curry f env -> curry (arr $ \x -> (f (Push x env)))

arr :: Term t => (Value t a -> Value t b) -> t '[a] b
arr f = undefined

compile :: Term t => Expr t '[] a -> t '[] a
compile (Expr v) = toTerm (v Empty)

instance Term t => Term (Expr t) where
  be (Expr x) (Expr f) = Expr $ \env -> f (Push (x env) env)
  tip = Expr $ \(Push h _) -> h
  const (Expr f) = Expr $ \(Push _ t) -> f t

  curry (Expr f) = Expr (Curry f)
  Expr f <*> Expr x = Expr $ \s -> let
    f' = f s
    x' = x s
    in case f' of
    Curry f'' env -> f'' (Push x' env)
    _ -> undefined

  u64 w = Expr $ Prelude.const $ Val64 w
  add = Expr $ Prelude.const Add

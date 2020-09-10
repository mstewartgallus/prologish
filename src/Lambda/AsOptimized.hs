{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Lambda.AsOptimized (Expr, optimize) where

import Control.Category
import Lambda
import Lambda.HasExp
import Lambda.HasProduct
import Lambda.Type
import Prelude hiding ((.), id, curry, uncurry, Either (..))

-- | The optimizer is based off the idea of normalization by
-- evaluation. Next this should really be moved back to the term level
-- representation
optimize :: Lambda k => Expr k a b -> k a b
optimize (Expr x) = compile x

newtype Expr k a b = Expr { apply :: forall env. Value k env a -> Value k env b }

data Value k env a where
  StuckValue :: Category k => k a b -> Value k env a -> Value k env b
  EnvValue :: Category k => Value k env env

  FnValue :: HasExp k => Value k env a -> (forall x. Value k x (b * a) -> Value k x c) -> Value k env (b ~> c)

  CoinValue :: HasProduct k => Value k env Unit
  PairValue :: HasProduct k => Value k env a -> Value k env b -> Value k env (a * b)

toExpr :: Lambda k => Value k env result -> k env result
toExpr expr = case expr of
  StuckValue hom x -> hom . toExpr x
  EnvValue -> id

  CoinValue -> unit
  PairValue f g -> toExpr f &&& toExpr g

  FnValue env f -> curry f' . toExpr env where
    f' = compile f

compile :: Lambda k => (forall env. Value k env a -> Value k env b) -> k a b
compile f = toExpr (f EnvValue)

instance Category k => Category (Expr k) where
  id = Expr id
  Expr f . Expr g = Expr (f . g)

instance HasProduct k => HasProduct (Expr k) where
  unit = Expr (const CoinValue)
  Expr f &&& Expr g = Expr $ \x -> PairValue (f x) (g x)
  first = Expr $ \x -> case x of
    PairValue l _ -> l
    _ -> StuckValue first x
  second = Expr $ \x -> case x of
    PairValue _ r -> r
    _ -> StuckValue second x

instance Lambda k => HasExp (Expr k) where
  curry (Expr f) = Expr (\x ->  FnValue x f)
  uncurry f = Expr (doUncurry f)

instance Lambda k => Lambda (Expr k) where
  u64 x = Expr (StuckValue $ u64 x)
  add = Expr (StuckValue add)

doUncurry :: Lambda k => Expr k a (b ~> c) -> Value k env (b * a) -> Value k env c
doUncurry (Expr f) x = let
  stuck = StuckValue (uncurry (compile f)) x
  in case x of
  PairValue b a -> case f a of
    FnValue env ep -> ep (PairValue b env)
    _ -> stuck
  _ -> stuck

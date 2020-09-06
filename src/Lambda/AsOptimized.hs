{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Lambda.AsOptimized (optimize) where

import Control.Category
import Lambda.HasExp
import Lambda.HasProduct
import Lambda.HasSum
import Lambda.Type
import Lambda.AsConcrete
import Prelude hiding ((.), id, curry, uncurry, Either (..))

optimize :: Category k => Expr k a b -> Expr k a b
optimize expr = compile (apply expr)

data Value k env a where
  StuckValue :: Category k => Expr k a b -> Value k env a -> Value k env b
  EnvValue :: Category k => Value k env env

  FnValue :: HasExp k => Value k env a -> (forall x. Value k x (b * a) -> Value k x c) -> Value k env (b ~> c)

  CoinValue :: HasProduct k => Value k env Unit
  PairValue :: HasProduct k => Value k env a -> Value k env b -> Value k env (a * b)

  LeftValue :: HasSum k => Value k env a -> Value k env (a + b)
  RightValue :: HasSum k => Value k env b -> Value k env (a + b)

apply :: Category k => Expr k b c -> Value k a b -> Value k a c
apply hom x = let
  stuck = StuckValue hom x
  in case hom of
  Id -> x
  f :.: g -> apply f (apply g x)

  Coin -> CoinValue

  Fanout f g -> PairValue (apply f x) (apply g x)
  First -> case x of
    PairValue l _ -> l
    _ -> stuck
  Second -> case x of
    PairValue _ r -> r
    _ -> stuck

  Fanin f g -> case x of
    LeftValue l -> apply f l
    RightValue r -> apply g r
    _ -> stuck
  Left -> LeftValue x
  Right -> RightValue x

  Curry f -> doCurry f x
  Uncurry f -> doUncurry f x

  _ -> stuck

doCurry :: HasExp k => Expr k (b * a) c -> Value k env a -> Value k env (b ~> c)
doCurry f env = FnValue env (apply f)

doUncurry :: HasExp k => Expr k a (b ~> c) -> Value k env (b * a) -> Value k env c
doUncurry f x = let
  stuck = StuckValue (Uncurry f) x
  in case x of
  PairValue b a -> case apply f a of
    FnValue env ep -> ep (PairValue b env)
    _ -> stuck
  _ -> stuck

toExpr :: Value k env result -> Expr k env result
toExpr expr = case expr of
  StuckValue hom x -> hom :.: toExpr x
  EnvValue -> Id

  CoinValue -> Coin
  PairValue f g -> Fanout (toExpr f) (toExpr g)

  LeftValue x -> Left :.: toExpr x
  RightValue x -> Right :.: toExpr x

  FnValue env f -> curry f' :.: toExpr env where
    f' = compile f

compile :: Category k => (forall env. Value k env a -> Value k env b) -> Expr k a b
compile f = toExpr (f EnvValue)

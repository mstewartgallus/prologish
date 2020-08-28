{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Lambda.Optimize (optimize) where

import Control.Category
import Control.Monad.State
import Lambda.Exp
import Lambda.Hoas
import Id (Stream (..))
import Lambda.Labels
import Lambda
import Lambda.Product
import Lambda.Sum
import Lambda.Type
import Lambda.Vars
import Lambda.AsConcrete
import Prelude hiding ((.), id, (&&&), (|||), curry, uncurry, Either (..))
import Data.Word
import Data.Typeable


data Value k env a where
  StuckValue :: Category k => Expr k a b -> Value k env a -> Value k env b
  EnvValue :: Category k => Value k env env

  FnValue :: Exp k => Expr k (b * a) c -> Value k env a -> Value k env (b ~> c)

  CoinValue :: Product k => Value k env Unit
  PairValue :: Product k => Value k env a -> Value k env b -> Value k env (a * b)

  LeftValue :: Sum k => Value k env a -> Value k env (a + b)
  RightValue :: Sum k => Value k env b -> Value k env (a + b)

apply :: Category k => Expr k a b -> Value k env a -> Value k env b
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

  Curry f -> FnValue f x
  Uncurry f -> case x of
    PairValue a b -> case apply f b of
      FnValue hom env -> apply hom (PairValue a env)
      _ -> stuck

  _ -> stuck

compile :: Category k => (forall env. Value k env a -> Value k env b) -> Expr k a b
compile f = toExpr (f EnvValue)

toExpr :: Value k env result -> Expr k env result
toExpr expr = case expr of
  StuckValue hom x -> hom :.: toExpr x
  EnvValue -> Id

  CoinValue -> Coin
  PairValue f g -> Fanout (toExpr f) (toExpr g)

  LeftValue x -> Left :.: toExpr x
  RightValue x -> Right :.: toExpr x

  FnValue f x -> curry f :.: toExpr x

optimize :: Category k => Expr k a b -> Expr k a b
optimize expr = compile (apply expr)

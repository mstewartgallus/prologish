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

optimize :: Category k => Expr k a b -> Expr k a b
optimize expr = compile (apply expr)

data Value k env a where
  StuckValue :: Category k => Expr k a b -> Value k env a -> Value k env b
  EnvValue :: Category k => Value k env env

  FnValue :: Exp k => Value k env a -> (forall x. Value k x (b * a) -> Value k x c) -> Value k env (b ~> c)

  CoinValue :: Product k => Value k env Unit
  PairValue :: Product k => Value k env a -> Value k env b -> Value k env (a * b)

  LeftValue :: Sum k => Value k env a -> Value k env (a + b)
  RightValue :: Sum k => Value k env b -> Value k env (a + b)

data Kont k a env where
  StuckKont :: Category k => Expr k a b -> Kont k b env -> Kont k a env
  EnvKont :: Category k => Kont k env env

  FnKont :: Exp k => Kont k a env -> (forall x. Kont k x b -> Kont k x c) -> Kont k (b ~> c) env

  AbsurdKont :: Sum k => Kont k Void env
  EitherKont :: Sum k => Kont k a env -> Kont k b env -> Kont k (a + b) env

  FirstKont :: Product k => Kont k a env -> Kont k (a * b) env
  SecondKont :: Product k => Kont k b env -> Kont k (a * b) env

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

doCurry :: Exp k => Expr k (b * a) c -> Value k env a -> Value k env (b ~> c)
doCurry f env = FnValue env (apply f)

doUncurry :: Exp k => Expr k a (b ~> c) -> Value k env (b * a) -> Value k env c
doUncurry f x = let
  stuck = StuckValue (Uncurry f) x
  in case x of
  PairValue b a -> case apply f a of
    FnValue env exp -> exp (PairValue b env)
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

applyK :: Category k => Expr k b a -> Kont k a x -> Kont k b x
applyK hom x = let
  stuck = StuckKont hom x
  in case hom of
  Id -> x
  f :.: g -> applyK g (applyK f x)

  Absurd -> AbsurdKont

  Fanin f g -> EitherKont (applyK f x) (applyK g x)
  Left -> case x of
    EitherKont l _ -> l
    _ -> stuck
  Right -> case x of
    EitherKont _ r -> r
    _ -> stuck

  Fanout f g -> case x of
    FirstKont l -> applyK f l
    SecondKont r -> applyK g r
    _ -> stuck
  First -> FirstKont x
  Second -> SecondKont x

  Uncurry f -> doUncurryK f x
  Curry f -> doCurryK f x

  _ -> stuck

doCurryK :: Exp k => Expr k (a * b) c -> Kont k (a ~> c) x -> Kont k b x
doCurryK f x = undefined

doUncurryK :: Exp k => Expr k a (b ~> c) -> Kont k c x -> Kont k (b * a) x
doUncurryK f x = undefined

toExprK :: Kont k arg env -> Expr k arg env
toExprK expr = case expr of
  StuckKont hom x -> toExprK x :.: hom
  EnvKont -> Id

  AbsurdKont -> Absurd
  EitherKont f g -> Fanin (toExprK f) (toExprK g)

  FirstKont x -> toExprK x :.: First
  SecondKont x -> toExprK x :.: Second

compileK :: Category k => (forall env. Kont k a env -> Kont k b env) -> Expr k b a
compileK f = toExprK (f EnvKont)

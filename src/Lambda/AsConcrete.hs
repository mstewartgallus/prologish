{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Lambda.AsConcrete (Expr (..), abstract) where

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
import Prelude hiding ((.), id, (&&&), (|||), curry, uncurry, Either (..))
import Data.Word

abstract :: Expr k a b -> k a b
abstract expr = case expr of
  Var x -> x
  Id -> id
  f :.: g -> abstract f . abstract g

  Coin -> unit
  First -> first
  Second -> second
  Fanout f g -> abstract f &&& abstract g

  Absurd -> absurd
  Left -> left
  Right -> right
  Fanin f g -> abstract f ||| abstract g

  Curry f -> curry (abstract f)
  Uncurry f -> uncurry (abstract f)

  ImplicitEnv t f x -> implicitEnv t (\x' -> abstract (f (Var x'))) (abstract x)
  ImplicitLabel t f x -> implicitLabel t (\x' -> abstract (f (Var x'))) (abstract x)

  Add -> add
  LitU64 x -> u64 x

data Expr k a b where
  Var :: k a b -> Expr k a b

  Id :: Category k => Expr k a a
  (:.:) :: Category k => Expr k b c -> Expr k a b -> Expr k a c

  Coin :: Product k => Expr k x Unit
  First :: Product k => Expr k (a * b) a
  Second :: Product k => Expr k (a * b) b
  Fanout :: Product k => Expr k c a -> Expr k c b -> Expr k c (a * b)

  Absurd :: Sum k => Expr k Void x
  Left :: Sum k => Expr k a (a + b)
  Right :: Sum k => Expr k b (a + b)
  Fanin :: Sum k => Expr k a c -> Expr k b c -> Expr k (a + b) c

  Curry :: Exp k => Expr k (a * env) b -> Expr k env (a ~> b)
  Uncurry :: Exp k => Expr k env (a ~> b) -> Expr k (a * env) b

  Add :: Lambda k => Expr k Unit (U64 ~> U64 ~> U64)
  LitU64 :: Lambda k =>  Word64 -> Expr k Unit U64

  ImplicitEnv :: Hoas k => ST a -> (Expr k Unit a  -> Expr k Unit b) -> Expr k env a -> Expr k env b
  ImplicitLabel :: Hoas k => ST a -> (Expr k a Void  -> Expr k b Void) -> Expr k a env -> Expr k b env

instance Category k => Category (Expr k) where
  id = Id
  (.) = (:.:)

instance Product k => Product (Expr k) where
  unit = Coin
  (&&&) = Fanout
  first = First
  second = Second

instance Sum k => Sum (Expr k) where
  absurd = Absurd
  (|||) = Fanin
  left = Left
  right = Right

instance Exp k => Exp (Expr k) where
  curry = Curry
  uncurry = Uncurry

instance Lambda k => Lambda (Expr k) where
  u64 x = LitU64 x
  add = Add

instance (Hoas k) => Hoas (Expr k) where
  implicitEnv = ImplicitEnv
  implicitLabel = ImplicitLabel

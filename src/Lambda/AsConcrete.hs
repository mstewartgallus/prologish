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

  Add -> add
  LitU64 x -> u64 x

data Expr k a b where
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

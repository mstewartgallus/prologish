{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsBound (Expr, bindPoints) where

import Control.Category
import Control.Monad.State
import Exp
import Hoas
import Labels
import Lambda
import Product
import Sum
import Type
import Vars
import Prelude hiding ((.), (<*>), id)

newtype Expr k (a :: T) (b :: T) = Expr {unExpr :: State Int (k a b)}

bindPoints :: Expr k env a -> k env a
bindPoints (Expr x) = evalState x 0

instance (Labels k, Vars k) => Hoas (Expr k) where
  var t f = Expr $ do
    n <- fresh
    let v = Var t n
    body <- unExpr (f (Expr $ pure (mkVar v)))
    pure (bindVar v body)
  label t f = Expr $ do
    n <- fresh
    let v = Label t n
    body <- unExpr (f (Expr $ pure (mkLabel v)))
    pure (bindLabel v body)

fresh :: State Int Int
fresh = do
  n <- get
  put (n + 1)
  return n

instance Category k => Category (Expr k) where
  id = Expr $ pure id
  Expr f . Expr g = Expr $ liftM2 (.) f g

instance Product k => Product (Expr k) where
  unit = Expr $ pure unit
  Expr f # Expr g = Expr $ liftM2 (#) f g
  first = Expr $ pure first
  second = Expr $ pure second

instance Sum k => Sum (Expr k) where
  absurd = Expr $ pure absurd
  Expr f ! Expr g = Expr $ liftM2 (!) f g
  left = Expr $ pure left
  right = Expr $ pure right

instance Exp k => Exp (Expr k) where
  lambda (Expr f) = Expr $ liftM lambda f
  unlambda (Expr f) = Expr $ liftM unlambda f
  eval = Expr $ pure eval

instance Lambda k => Lambda (Expr k) where
  u64 x = Expr $ pure (u64 x)
  add = Expr $ pure add

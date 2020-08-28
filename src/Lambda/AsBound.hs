{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Lambda.AsBound (Expr, bindPoints) where

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
import Prelude hiding ((.), id, (&&&), (|||), curry, uncurry)

newtype Expr k (a :: T) (b :: T) = Expr {unExpr :: Stream -> k a b}

bindPoints :: Stream -> Expr k env a -> k env a
bindPoints str (Expr x) = x str

instance (Labels k, Vars k) => Hoas (Expr k) where
  implicitEnv t f (Expr x) = Expr $ \(Stream n bodys xs) ->
    bindImplicitEnv n t (\v -> unExpr (f (Expr $ const v)) bodys) (x xs)

  implicitLabel t f (Expr x) = Expr $ \(Stream n bodys xs) ->
    bindImplicitLabel n t (\v -> unExpr (f (Expr $ const v)) bodys) (x xs)

instance Category k => Category (Expr k) where
  id = Expr $ pure id
  Expr f . Expr g = Expr $ liftM2 (.) f g

instance Product k => Product (Expr k) where
  unit = Expr $ pure unit
  Expr f &&& Expr g = Expr $ liftM2 (&&&) f g
  first = Expr $ pure first
  second = Expr $ pure second

instance Sum k => Sum (Expr k) where
  absurd = Expr $ pure absurd
  Expr f ||| Expr g = Expr $ liftM2 (|||) f g
  left = Expr $ pure left
  right = Expr $ pure right

instance Exp k => Exp (Expr k) where
  curry (Expr f) = Expr $ liftM curry f
  uncurry (Expr f) = Expr $ liftM uncurry f

instance Lambda k => Lambda (Expr k) where
  u64 x = Expr $ pure (u64 x)
  add = Expr $ pure add

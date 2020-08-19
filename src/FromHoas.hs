{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module FromHoas (Expr, fromHoas) where

import Control.Category
import Control.Monad.State
import Data.Typeable ((:~:) (..))
import Hoas
import Lam
import Type
import Prelude hiding ((.), id)

fromHoas :: Lam k => Expr (Pf k) a b -> k a b
fromHoas = emit . bind

data Var a = Var (ST a) Int

eqVar :: Var a -> Var b -> Maybe (a :~: b)
eqVar (Var t m) (Var t' n)
  | m == n = eqT t t'
  | otherwise = Nothing

data Label a = Label (ST a) Int

eqLabel :: Label a -> Label b -> Maybe (a :~: b)
eqLabel (Label t m) (Label t' n)
  | m == n = eqT t t'
  | otherwise = Nothing

newtype Expr k (a :: T) (b :: T) = Expr {unExpr :: State Int (k a b)}

instance Bindable k => Hoas (Expr k) where
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

instance Lam k => Lam (Expr k) where
  Expr f # Expr g = Expr $ liftM2 (#) f g
  first = Expr $ pure first
  second = Expr $ pure second

  Expr f ! Expr g = Expr $ liftM2 (!) f g
  left = Expr $ pure left
  right = Expr $ pure right

  lambda (Expr f) = Expr $ liftM lambda f
  eval = Expr $ pure eval

  u64 x = Expr $ pure (u64 x)
  add = Expr $ pure add

class Lam k => Bindable k where
  mkVar :: Var a -> k x a
  mkLabel :: Label a -> k a x

  bindVar :: Var a -> k env b -> k (env * a) b
  bindLabel :: Label b -> k env a -> k env (a + b)

data Pf k a b where
  PfVar :: Var a -> Pf k x a
  PfLabel :: Label a -> Pf k a x
  PfCompose :: Pf k b c -> Pf k a b -> Pf k a c
  PfFactor :: Pf k c a -> Pf k c b -> Pf k c (a * b)
  PfFactorSum :: Pf k a c -> Pf k b c -> Pf k (a + b) c
  PfLambda :: Pf k (env * a) b -> Pf k env (a ~> b)
  PfPure :: k a b -> Pf k a b

instance Category k => Category (Pf k) where
  id = PfPure id
  (.) = PfCompose

instance Lam k => Bindable (Pf k) where
  mkVar = PfVar
  mkLabel = PfLabel
  bindVar = abstractVar
  bindLabel = abstractLabel

instance Lam k => Lam (Pf k) where
  f # g = PfFactor f g
  first = PfPure first
  second = PfPure second

  f ! g = PfFactorSum f g
  left = PfPure left
  right = PfPure right

  lambda f = PfLambda f
  eval = PfPure eval

  u64 x = PfPure (u64 x)
  add = PfPure add

bind :: Expr k env a -> k env a
bind (Expr x) = evalState x 0

pointFree :: Lam k => Bound k env a -> Pf k env a
pointFree expr = case expr of
  BoundVar x -> PfVar x
  BoundBind x body -> abstractVar x (pointFree body)
  BoundLabel x -> PfLabel x
  BoundBindLabel x body -> abstractLabel x (pointFree body)
  BoundFactor f g -> PfFactor (pointFree f) (pointFree g)
  BoundLambda f -> PfLambda (pointFree f)
  BoundPure k -> PfPure k
  BoundCompose f g -> PfCompose (pointFree f) (pointFree g)

abstractVar :: Lam k => Var a -> Pf k env b -> Pf k (env * a) b
abstractVar m expr = case expr of
  p@(PfVar n) -> case m `eqVar` n of
    Just Refl -> PfPure second
    Nothing -> p . PfPure first
  PfPure k -> PfPure k . PfPure first
  PfCompose f g -> f' . PfFactor g' (PfPure second)
    where
      f' = abstractVar m f
      g' = abstractVar m g
  PfFactor f g -> PfFactor f' g'
    where
      f' = abstractVar m f
      g' = abstractVar m g
  PfLambda f -> PfLambda (f' . flp)
    where
      flp = PfFactor (PfFactor getenv geta) getb
      geta = PfPure second
      getb = PfPure second . PfPure first
      getenv = PfPure first . PfPure first
      f' = abstractVar m f

abstractLabel :: Lam k => Label a -> Pf k env b -> Pf k env (b + a)
abstractLabel m expr = case expr of
  p@(PfLabel n) -> case m `eqLabel` n of
    Just Refl -> PfPure right
    Nothing -> PfPure left . p
  PfPure k -> PfPure left . PfPure k
  PfCompose f g -> PfFactorSum f' (PfPure right) . g'
    where
      f' = abstractLabel m f
      g' = abstractLabel m g

emit :: Lam k => Pf k a b -> k a b
emit expr = case expr of
  PfPure k -> k
  PfCompose f g -> emit f . emit g
  PfFactor f g -> emit f # emit g
  PfLambda f -> lambda (emit f)

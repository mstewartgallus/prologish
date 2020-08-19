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

fromHoas :: Lam k => Expr k a b -> k a b
fromHoas = emit . pointFree . bind

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

newtype Expr k a b = Expr {unExpr :: State Int (Bound k a b)}

instance Lam k => Hoas (Expr k) where
  var t f = Expr $ do
    n <- fresh
    let v = Var t n
    body <- unExpr (f (Expr $ pure (BoundVar v)))
    pure (BoundBind v body)
  label t f = Expr $ do
    n <- fresh
    let v = Label t n
    body <- unExpr (f (Expr $ pure (BoundLabel v)))
    pure (BoundBindLabel v body)

fresh :: State Int Int
fresh = do
  n <- get
  put (n + 1)
  return n

instance Category k => Category (Expr k) where
  id = Expr $ pure $ BoundPure id
  Expr f . Expr g = Expr $ liftM2 BoundCompose f g

instance Lam k => Lam (Expr k) where
  Expr f # Expr g = Expr $ liftM2 BoundFactor f g
  first = Expr $ pure (BoundPure first)
  second = Expr $ pure (BoundPure second)

  Expr f ! Expr g = Expr $ liftM2 BoundFactorSum f g
  left = Expr $ pure (BoundPure left)
  right = Expr $ pure (BoundPure right)

  lambda (Expr f) = Expr $ liftM BoundLambda f
  eval = Expr $ pure (BoundPure eval)

  u64 x = Expr $ pure (BoundPure (u64 x))
  add = Expr $ pure (BoundPure add)

data Bound k a b where
  BoundVar :: Var a -> Bound k x a
  BoundLabel :: Label a -> Bound k a x
  BoundBind :: Var a -> Bound k env b -> Bound k (env * a) b
  BoundBindLabel :: Label b -> Bound k env a -> Bound k env (a + b)
  BoundCompose :: Bound k b c -> Bound k a b -> Bound k a c
  BoundFactor :: Bound k c a -> Bound k c b -> Bound k c (a * b)
  BoundFactorSum :: Bound k a c -> Bound k b c -> Bound k (a + b) c
  BoundLambda :: Bound k (env * a) b -> Bound k env (a ~> b)
  BoundPure :: k a b -> Bound k a b

instance Category k => Category (Bound k) where
  id = BoundPure id
  (.) = BoundCompose

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

bind :: Category k => Expr k env a -> Bound k env a
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

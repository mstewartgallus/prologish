{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Expr (Expr, var, label, fn, compile, view) where

import Control.Category
import Control.Monad.State
import Data.Typeable ((:~:) (..))
import Lam
import Type
import View
import Prelude hiding ((.), fn, id, label, var)

view :: Expr View a b -> String
view = view' . bind

view' :: Bound View a b -> String
view' expr = case expr of
  BoundVar (Var _ n) -> "v" ++ show n
  BoundBind (Var _ n) x -> "v" ++ show n ++ "." ++ indent ("\n" ++ view' x)
  BoundLabel (Label _ n) -> "l" ++ show n
  BoundBindLabel (Label _ n) x -> "l" ++ show n ++ "." ++ indent ("\n" ++ view' x)
  BoundCompose f g -> view' g ++ "\n" ++ view' f
  BoundFactor f g -> view' g ++ " Δ " ++ view' f
  BoundLambda f -> "λ " ++ view' f
  BoundPure x -> show x

indent :: String -> String
indent = unlines . map ('\t' :) . lines

var :: KnownT a => (Object (Expr k) a -> Expr k env b) -> Expr k (env * a) b
var f = ExprBindVar inferT f

label :: KnownT a => (Point (Expr k) a -> Expr k b env) -> Expr k b (env + a)
label f = ExprBindLabel inferT f

fn :: (KnownT a, Lam k) => (Object (Expr k) a -> Expr k env b) -> Expr k env (a ~> b)
fn f = lambda (var f)

compile :: Lam k => Expr k a b -> k a b
compile = emit . pointFree . bind

data Var a = Var (ST a) Int

data Label a = Label (ST a) Int

eqVar :: Var a -> Var b -> Maybe (a :~: b)
eqVar (Var t m) (Var t' n)
  | m == n = eqT t t'
  | otherwise = Nothing

eqLabel :: Label a -> Label b -> Maybe (a :~: b)
eqLabel (Label t m) (Label t' n)
  | m == n = eqT t t'
  | otherwise = Nothing

data Expr k a b where
  ExprId :: Expr k a a
  ExprPure :: k a b -> Expr k a b
  ExprCompose :: Expr k b c -> Expr k a b -> Expr k a c

  ExprVar :: Var a -> Expr k x a
  ExprBindVar :: ST a -> (Object (Expr k) a -> Expr k env b) -> Expr k (env * a) b
  ExprFactor :: Expr k c a -> Expr k c b -> Expr k c (a * b)

  ExprLabel :: Label a -> Expr k a x
  ExprBindLabel :: ST a -> (Point (Expr k) a -> Expr k b env) -> Expr k b (env + a)
  ExprFactorSum :: Expr k a c -> Expr k b c -> Expr k (a + b) c

  ExprLambda :: Expr k (env * a) b -> Expr k env (a ~> b)

expr :: k a b -> Expr k a b
expr = ExprPure

instance Category (Expr k) where
  id = ExprId
  (.) = ExprCompose

instance Lam k => Lam (Expr k) where
  (#) = ExprFactor
  first = expr first
  second = expr second

  (!) = ExprFactorSum
  left = expr left
  right = expr right

  lambda = ExprLambda
  eval = expr eval

  u64 x = expr (u64 x)
  add = expr add

data Bound k a b where
  BoundVar :: Var a -> Bound k x a
  BoundLabel :: Label a -> Bound k a x
  BoundBind :: Var a -> Bound k env b -> Bound k (env * a) b
  BoundBindLabel :: Label b -> Bound k env a -> Bound k env (a + b)
  BoundCompose :: Bound k b c -> Bound k a b -> Bound k a c
  BoundFactor :: Bound k c a -> Bound k c b -> Bound k c (a * b)
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
bind x = evalState (bind' x) 0

bind' :: Category k => Expr k env a -> State Int (Bound k env a)
bind' expr = case expr of
  ExprVar x -> return (BoundVar x)
  ExprId -> return (BoundPure id)
  ExprPure f -> return (BoundPure f)
  ExprCompose f g -> do
    f' <- bind' f
    g' <- bind' g
    return (f' . g')
  ExprFactor f g -> do
    f' <- bind' f
    g' <- bind' g
    return (BoundFactor f' g')
  ExprLambda f -> do
    f' <- bind' f
    return (BoundLambda f')
  ExprBindLabel t f -> do
    n <- get
    put (n + 1)
    let x = Label t n
    y <- bind' (f (ExprLabel x))
    return (BoundBindLabel x y)
  ExprBindVar t f -> do
    n <- get
    put (n + 1)
    let x = Var t n
    y <- bind' (f (ExprVar x))
    return (BoundBind x y)

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

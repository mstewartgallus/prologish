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
import Exp
import Hoas
import Lambda
import Product
import Sum
import Type
import Prelude hiding ((.), (<*>), id)

fromHoas :: Lambda k => Expr (Varless (Labeless k)) a b -> k a b
fromHoas = removeLabels . removeVars . bind

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

bind :: Expr k env a -> k env a
bind (Expr x) = evalState x 0

instance Vars k => Hoas (Expr k) where
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

instance Lambda k => Product (Expr k) where
  unit = Expr $ pure unit
  Expr f # Expr g = Expr $ liftM2 (#) f g
  first = Expr $ pure first
  second = Expr $ pure second

instance Lambda k => Sum (Expr k) where
  absurd = Expr $ pure absurd
  Expr f ! Expr g = Expr $ liftM2 (!) f g
  left = Expr $ pure left
  right = Expr $ pure right

instance Lambda k => Exp (Expr k) where
  lambda (Expr f) = Expr $ liftM lambda f
  eval = Expr $ pure eval

instance Lambda k => Lambda (Expr k) where
  u64 x = Expr $ pure (u64 x)
  add = Expr $ pure add

class Lambda k => Labels k where
  mkLabel :: Label a -> k a x
  bindLabel :: Label b -> k env a -> k env (a + b)

class Labels k => Vars k where
  mkVar :: Var a -> k x a
  bindVar :: Var a -> k env b -> k (env * a) b

newtype Varless k a (b :: T) = V (forall env. Env env -> k (a * env) b)

data Env a where
  EmptyEnv :: Env Unit
  VarEnv :: Var v -> Env a -> Env (a * v)

instance Lambda k => Category (Varless k) where
  id = V $ const first
  V f . V g = V $ \env -> f env . (g env # second)

instance Labels k => Labels (Varless k) where
  mkLabel lbl = V $ const (mkLabel lbl . first)
  bindLabel lbl (V x) = V $ \env -> bindLabel lbl (x env)

instance Labels k => Vars (Varless k) where
  mkVar v = V $ \env -> matchVar v env . second
  bindVar v (V x) = V $ \env ->
    let shuffle :: Labels k => k ((a * c) * b) (a * (b * c))
        shuffle = (first . first) # (second # (second . first))
     in x (VarEnv v env) . shuffle

matchVar :: Lambda k => Var a -> Env env -> k env a
matchVar x (VarEnv y rest) = case x `eqVar` y of
  Just Refl -> second
  Nothing -> matchVar x rest . first

instance Lambda k => Product (Varless k) where
  unit = V $ const unit
  V f # V g = V $ \env -> f env # g env
  first = V $ const (first . first)
  second = V $ const (second . first)

instance Lambda k => Sum (Varless k) where
  V f ! V g = V $ \env -> factor (f env) (g env)
  left = V $ const (left . first)
  right = V $ const (right . first)

instance Lambda k => Exp (Varless k) where
  lambda (V f) = V $ \env ->
    let shuffle :: Lambda k => k ((a * b) * c) ((a * c) * b)
        shuffle = ((first . first) # second) # (second . first)
     in lambda (f env . shuffle)
  eval = V $ const (eval . first)

instance Lambda k => Lambda (Varless k) where
  u64 x = V $ const (u64 x)
  add = V $ const add

removeVars :: Lambda k => Varless k a b -> k a b
removeVars (V x) = x EmptyEnv . (id # unit)

newtype Labeless k (a :: T) b = L (forall c. Case c -> k a (b + c))

data Case a where
  EmptyCase :: Case Void
  LabelCase :: Label l -> Case a -> Case (a + l)

instance Lambda k => Category (Labeless k) where
  id = L $ const left
  L f . L g = L $ \env -> (f env ! right) . g env

instance Lambda k => Labels (Labeless k) where
  mkLabel v = L $ \env -> right . matchLabels v env
  bindLabel v (L x) = L $ \env ->
    let shuffle :: Lambda k => k (a + (c + b)) ((a + b) + c)
        shuffle = undefined
     in shuffle . x (LabelCase v env)

instance Lambda k => Product (Labeless k) where
  unit = L $ const (left . unit)
  L f # L g = L $ \env -> distribute (f env) (g env)
  first = L $ const (left . first)
  second = L $ const (left . second)

instance Lambda k => Sum (Labeless k) where
  absurd = L $ const (left . absurd)
  L f ! L g = L $ \env -> f env ! g env
  left = L $ const (left . left)
  right = L $ const (left . right)

instance Lambda k => Exp (Labeless k) where
  eval = L $ const (left . eval)

instance Lambda k => Lambda (Labeless k) where
  u64 x = L $ const (left . u64 x)
  add = L $ const (left . add)

factor :: Lambda k => k (a * x) c -> k (b * x) c -> k ((a + b) * x) c
factor f g = unlambda (lambda f ! lambda g)

distribute :: Lambda k => k c (a + x) -> k c (b + x) -> k c ((a * b) + x)
distribute f g = unlambda (lambda (unlambda bar . (second # first)) ! lambda (right . first)) . (f # g)

bar :: Lambda k => k (b + x) (a ~> ((a * b) + x))
bar = lambda (left . (second # first)) ! lambda (right . first)

matchLabels :: Lambda k => Label a -> Case c -> k a c
matchLabels x (LabelCase y rest) = case x `eqLabel` y of
  Just Refl -> right
  Nothing -> left . matchLabels x rest

removeLabels :: Lambda k => Labeless k a b -> k a b
removeLabels (L x) = (id ! absurd) . x EmptyCase

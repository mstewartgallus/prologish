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

fromHoas :: Lam k => Expr (Varless (Labeless k)) a b -> k a b
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

class Lam k => Labels k where
  mkLabel :: Label a -> k a x
  bindLabel :: Label b -> k env a -> k env (a + b)

class Labels k => Vars k where
  mkVar :: Var a -> k x a
  bindVar :: Var a -> k env b -> k (env * a) b

newtype Varless k a (b :: T) = V (forall env. Env env -> k (env * a) b)

data Env a where
  EmptyEnv :: Env Unit
  VarEnv :: Var v -> Env a -> Env (a * v)

instance Lam k => Category (Varless k) where
  id = V $ const second
  V f . V g = V $ \env -> f env . first # g env

instance Labels k => Labels (Varless k) where
  mkLabel lbl = V $ const (mkLabel lbl . second)
  bindLabel lbl (V x) = V $ \env -> bindLabel lbl (x env)

instance Labels k => Vars (Varless k) where
  mkVar v = V $ \env -> matchVar v env . first
  bindVar v (V x) = V $ \env ->
    let shuffle :: Labels k => k (a * (c * b)) ((a * b) * c)
        shuffle = (first # (second . second)) # (first . second)
     in x (VarEnv v env) . shuffle

matchVar :: Lam k => Var a -> Env env -> k env a
matchVar x (VarEnv y rest) = case x `eqVar` y of
  Just Refl -> second
  Nothing -> matchVar x rest . first

instance Lam k => Lam (Varless k) where
  V f # V g = V $ \env -> f env # g env
  first = V $ const (first . second)
  second = V $ const (second . second)

  left = V $ const (left . second)
  right = V $ const (right . second)

  lambda (V f) = V $ \env ->
    let shuffle :: Lam k => k ((a * b) * c) (a * (b * c))
        shuffle = (first . first) # ((second . first) # second)
     in lambda (f env . shuffle)
  eval = V $ const (eval . second)

  u64 x = V $ const (u64 x)
  add = V $ const add

removeVars :: Lam k => Varless k a b -> k a b
removeVars (V x) = x EmptyEnv . (unit # id)

data Labeless k a b where
  LabelessLabel :: Label a -> Labeless k a x
  LabelessCompose :: Labeless k b c -> Labeless k a b -> Labeless k a c
  LabelessFactor :: Labeless k c a -> Labeless k c b -> Labeless k c (a * b)
  LabelessFactorSum :: Labeless k a c -> Labeless k b c -> Labeless k (a + b) c
  LabelessLambda :: Labeless k (env * a) b -> Labeless k env (a ~> b)
  LabelessPure :: k a b -> Labeless k a b

instance Category k => Category (Labeless k) where
  id = LabelessPure id
  (.) = LabelessCompose

instance Lam k => Labels (Labeless k) where
  mkLabel = LabelessLabel
  bindLabel = abstractLabel

instance Lam k => Lam (Labeless k) where
  unit = LabelessPure unit
  absurd = LabelessPure absurd

  f # g = LabelessFactor f g
  first = LabelessPure first
  second = LabelessPure second

  f ! g = LabelessFactorSum f g
  left = LabelessPure left
  right = LabelessPure right

  lambda f = LabelessLambda f
  eval = LabelessPure eval

  u64 x = LabelessPure (u64 x)
  add = LabelessPure add

abstractLabel :: Lam k => Label a -> Labeless k env b -> Labeless k env (b + a)
abstractLabel m expr = case expr of
  p@(LabelessLabel n) -> case m `eqLabel` n of
    Just Refl -> LabelessPure right
    Nothing -> LabelessPure left . p
  LabelessPure k -> LabelessPure left . LabelessPure k
  LabelessCompose f g -> LabelessFactorSum f' (LabelessPure right) . g'
    where
      f' = abstractLabel m f
      g' = abstractLabel m g

removeLabels :: Lam k => Labeless k a b -> k a b
removeLabels expr = case expr of
  LabelessPure k -> k
  LabelessCompose f g -> removeLabels f . removeLabels g
  LabelessFactor f g -> removeLabels f # removeLabels g
  LabelessLambda f -> lambda (removeLabels f)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsVarless (Varless, removeVariables) where

import Control.Category
import Control.Monad.State
import Data.Typeable ((:~:) (..))
import Exp
import Hoas
import Labels
import Lambda
import Product
import Sum
import Type
import Vars
import Prelude hiding ((.), (<*>), id)

removeVariables :: Product k => Varless k a b -> k a b
removeVariables (V x) = x EmptyEnv . (id # unit)

matchVar :: Product k => Var a -> Env env -> k env a
matchVar x (VarEnv y rest) = case x `eqVar` y of
  Just Refl -> second
  Nothing -> matchVar x rest . first

newtype Varless k a (b :: T) = V (forall env. Env env -> k (a * env) b)

data Env a where
  EmptyEnv :: Env Unit
  VarEnv :: Var v -> Env a -> Env (a * v)

instance Product k => Category (Varless k) where
  id = V $ const first
  V f . V g = V $ \env -> f env . (g env # second)

instance (Exp k, Labels k) => Labels (Varless k) where
  mkLabel lbl = V $ const (mkLabel lbl . first)
  bindLabel lbl (V x) = V $ \env -> bindLabel lbl (x env)

instance (Product k, Labels k) => Vars (Varless k) where
  mkVar v = V $ \env -> matchVar v env . second
  bindVar v (V x) = V $ \env ->
    let shuffle :: Product k => k ((a * c) * b) (a * (b * c))
        shuffle = (first . first) # (second # (second . first))
     in x (VarEnv v env) . shuffle

instance Product k => Product (Varless k) where
  unit = V $ const unit
  V f # V g = V $ \env -> f env # g env
  first = V $ const (first . first)
  second = V $ const (second . first)

instance (Sum k, Exp k) => Sum (Varless k) where
  V f ! V g = V $ \env -> factor (f env) (g env)
  left = V $ const (left . first)
  right = V $ const (right . first)

instance Exp k => Exp (Varless k) where
  lambda (V f) = V $ \env ->
    let shuffle :: Product k => k ((a * b) * c) ((a * c) * b)
        shuffle = ((first . first) # second) # (second . first)
     in lambda (f env . shuffle)
  eval = V $ const (eval . first)

instance Lambda k => Lambda (Varless k) where
  u64 x = V $ const (u64 x)
  add = V $ const add

factor :: (Sum k, Exp k) => k (a * x) c -> k (b * x) c -> k ((a + b) * x) c
factor f g = unlambda (lambda f ! lambda g)

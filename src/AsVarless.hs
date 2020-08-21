{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsVarless (Varless, removeVariables) where

import Control.Category
import Data.Kind
import Data.Typeable ((:~:) (..))
import Exp
import Id (Id)
import Labels
import Lambda
import Product
import Sum
import Type
import Vars
import Prelude hiding ((.), (<*>), id)

removeVariables :: Product k => Varless k a b -> k a b
removeVariables (V x) = x emptyEnv . (id # unit)

newtype Varless k a (b :: T) = V (forall env. Env k env -> k (a * env) b)

data Var a = Var (ST a) Id

eqVar :: Var a -> Var b -> Maybe (a :~: b)
eqVar (Var t m) (Var t' n)
  | m == n = eqT t t'
  | otherwise = Nothing

newtype Env (k :: T -> T -> Type) env = Env (forall a. Var a -> k env a)

emptyEnv :: Env k Unit
emptyEnv = Env (const undefined)

addEnv :: Product k => Env k env -> Var a -> Env k (env * a)
addEnv env v = Env $ \maybeV -> case maybeV `eqVar` v of
  Just Refl -> second
  Nothing -> findVar maybeV env . first

findVar :: Var a -> Env k env -> k env a
findVar x (Env f) = f x

inV :: Product k => k a b -> Varless k a b
inV f = V (const (f . first))

instance Product k => Category (Varless k) where
  id = inV id
  V f . V g = V $ \env -> f env . (g env # second)

instance (Product k, Labels k) => Labels (Varless k) where
  bindMapLabel n t f = V $ \env -> bindMapLabel n t $ \v ->
    case f (inV v) of
      V x -> x env

instance Product k => Vars (Varless k) where
  bindMapVar n t f =
    let v = Var t n
        varExpr = V $ \env -> findVar v env . second
     in V $ \env -> case f varExpr of
          V x ->
            let shuffle :: Product k => k (c * b) (Unit * (b * c))
                shuffle = unit # (second # first)
             in x (addEnv env v) . shuffle

instance Product k => Product (Varless k) where
  unit = inV unit
  V f # V g = V $ \env -> f env # g env
  first = inV first
  second = inV second

instance (Sum k, Exp k) => Sum (Varless k) where
  absurd = inV absurd
  V f ! V g = V $ \env -> factor (f env) (g env)
  left = inV left
  right = inV right

instance Exp k => Exp (Varless k) where
  lambda (V f) = V $ \env ->
    let shuffle :: Product k => k ((a * b) * c) ((a * c) * b)
        shuffle = ((first . first) # second) # (second . first)
     in lambda (f env . shuffle)
  eval = inV eval

instance Lambda k => Lambda (Varless k) where
  u64 x = inV (u64 x)
  add = inV add

factor :: (Sum k, Exp k) => k (a * x) c -> k (b * x) c -> k ((a + b) * x) c
factor f g = eval . (((lambda f ! lambda g) . first) # second)

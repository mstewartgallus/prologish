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

removeVariables :: Varless k a b -> k a b
removeVariables = outV

data Varless k a (b :: T) = V
  { outV :: k a b,
    pointFree :: forall v. Var v -> Varless k (a * v) b
  }

data Var a = Var (ST a) Id

eqVar :: Var a -> Var b -> Maybe (a :~: b)
eqVar (Var t m) (Var t' n)
  | m == n = eqT t t'
  | otherwise = Nothing

inV :: Product k => k a b -> Varless k a b
inV x = me
  where
    me =
      V
        { outV = x,
          pointFree = const (me . first)
        }

instance Product k => Category (Varless k) where
  id = inV id
  f . g = me
    where
      me =
        V
          { outV = outV f . outV g,
            pointFree = \v -> pointFree f v . (pointFree g v # second)
          }

instance (Product k, Labels k) => Labels (Varless k) where
  bindMapLabel n t f = undefined

instance Product k => Vars (Varless k) where
  bindMapVar n t f = me
    where
      me =
        V
          { outV = outV body,
            pointFree = pointFree body
          }
      v = Var t n
      body = pointFree (f (mkVar v)) v . (unit # id)

mkVar :: Product k => Var a -> Varless k Unit a
mkVar v@(Var _ n) = me
  where
    me =
      V
        { outV = error ("free variable " ++ show n),
          pointFree = \maybeV -> case eqVar v maybeV of
            Nothing -> me . first
            Just Refl -> inV second
        }

instance Product k => Product (Varless k) where
  unit = inV unit
  f # g = me
    where
      me =
        V
          { outV = outV f # outV g,
            pointFree = \v -> pointFree f v # pointFree g v
          }
  first = inV first
  second = inV second

instance (Sum k, Exp k) => Sum (Varless k) where
  absurd = inV absurd
  f ! g = me
    where
      me =
        V
          { outV = outV f ! outV g,
            pointFree = \v -> factor (pointFree f v) (pointFree g v)
          }
  left = inV left
  right = inV right

instance Exp k => Exp (Varless k) where
  lambda f = me
    where
      me =
        V
          { outV = lambda (outV f),
            pointFree = \v -> lambda (pointFree f v . shuffle)
          }
      shuffle :: Product k => k ((a * c) * b) ((a * b) * c)
      shuffle = ((first . first) # second) # (second . first)
  eval = inV eval

instance Lambda k => Lambda (Varless k) where
  u64 x = inV (u64 x)
  add = inV add

factor :: (Sum k, Exp k) => k (a * x) c -> k (b * x) c -> k ((a + b) * x) c
factor f g = eval . (((lambda f ! lambda g) . first) # second)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsFn (PointFree, pointFree) where

import Control.Category
import Data.Kind
import Data.Maybe
import Data.Typeable ((:~:) (..))
import Fn (Fn)
import qualified Fn
import Id (Id)
import Lambda
import Lambda.Exp
import qualified Term.Bound as Bound
import Term.Type
import Prelude hiding (curry, id, uncurry, (&&&), (.), (<*>))

pointFree :: PointFree k a -> k '[] a
pointFree (PointFree x) = out x

newtype PointFree k a = PointFree (Pf k '[] a)

instance Fn k => Bound.Bound (PointFree k) where
  PointFree f <*> PointFree x = PointFree me
    where
      me =
        V
          { out = out f Fn.<*> out x,
            removeVar = \v -> case (removeVar f v, removeVar x v) of
              (Just f', Just x') -> Just (f' Fn.<*> x')
              (_, Just x') -> Just (Fn.tail f Fn.<*> x')
              (Just f', _) -> Just (f' Fn.<*> Fn.tail x)
              _ -> Nothing
          }

  lam id t f = PointFree (Fn.curry me)
    where
      v = Var t id
      PointFree body = f (PointFree (mkVar v))
      me = case removeVar body v of
        Nothing -> Fn.tail body
        Just y -> y

  u64 x = PointFree (to (Fn.u64 x))
  add = PointFree (to Fn.add)

instance Fn k => Fn (Pf k) where
  f <*> x = me
    where
      me =
        V
          { out = out f Fn.<*> out x,
            removeVar = \v -> case (removeVar f v, removeVar x v) of
              (Just f', Just x') -> Just (f' Fn.<*> x')
              (_, Just x') -> Just (Fn.tail f Fn.<*> x')
              (Just f', _) -> Just (f' Fn.<*> Fn.tail x)
              _ -> Nothing
          }
  curry f = me
    where
      me =
        V
          { out = Fn.curry (out f),
            removeVar = \v -> case removeVar f v of
              Nothing -> Nothing
              Just f' -> Just (Fn.curry (swap f'))
          }
      swap :: Fn k => k (x ': a ': env) b -> k (a ': x ': env) b
      swap = undefined

data Pf k env (b :: T) = V
  { out :: k env b,
    removeVar :: forall v. Var v -> Maybe (Pf k (v ': env) b)
  }

data Var a = Var (ST a) Id

eqVar :: Var a -> Var b -> Maybe (a :~: b)
eqVar (Var t m) (Var t' n)
  | m == n = eqT t t'
  | otherwise = Nothing

to :: k a b -> Pf k a b
to x = me
  where
    me =
      V
        { out = x,
          removeVar = const Nothing
        }

mkVar :: Fn k => Var a -> Pf k '[] a
mkVar v@(Var _ n) = me
  where
    me =
      V
        { out = error ("free variable " ++ show n),
          removeVar = \maybeV -> case eqVar v maybeV of
            Nothing -> Nothing
            Just Refl -> Just (to Fn.head)
        }

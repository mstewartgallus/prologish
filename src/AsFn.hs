{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsFn (PointFree, pointFree) where

import Data.Maybe
import Data.Typeable ((:~:) (..))
import Fn (Fn)
import qualified Fn
import Id (Id)
import qualified Term.Bound as Bound
import Term.Type
import Prelude hiding (curry, id, uncurry, (.), (<*>))

pointFree :: PointFree k a -> k '[] a
pointFree (PointFree x) = out x

newtype PointFree k a = PointFree (Pf k '[] a)

instance Fn k => Bound.Bound (PointFree k) where
  PointFree f <*> PointFree x = PointFree (f Fn.<*> x)

  be n (PointFree x) t f = PointFree (Fn.be x me)
    where
      v = Var t n
      PointFree body = f (PointFree (mkVar v))
      me = case removeVar body v of
        Nothing -> Fn.tail body
        Just y -> y

  lam n t f = PointFree (Fn.curry me)
    where
      v = Var t n
      PointFree body = f (PointFree (mkVar v))
      me = case removeVar body v of
        Nothing -> Fn.tail body
        Just y -> y

  u64 x = PointFree (to (Fn.u64 x))
  add = PointFree (to Fn.add)

instance Fn k => Fn (Pf k) where
  u64 x = to (Fn.u64 x)
  add = to Fn.add

  head = me
    where
      me =
        V
          { out = Fn.head,
            removeVar = const Nothing
          }
  tail f = me
    where
      me =
        V
          { out = Fn.tail (out f),
            removeVar = \v -> case removeVar f v of
              Just f' -> Just (Fn.swap (Fn.tail f'))
              _ -> Nothing
          }

  be x f = me
    where
      me =
        V
          { out = out x `Fn.be` out f,
            removeVar = \v -> case (removeVar x v, removeVar f v) of
              (Just x', Just f') -> Just (x' `Fn.be` Fn.swap f')
              (_, Just f') -> Just (Fn.tail x `Fn.be` Fn.swap f')
              (Just x', _) -> Just (x' `Fn.be` Fn.swap (Fn.tail f))
              _ -> Nothing
          }

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
              Just f' -> Just (Fn.curry (Fn.swap f'))
          }

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

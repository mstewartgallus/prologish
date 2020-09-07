{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsTerm (PointFree, pointFree) where

import Data.Maybe
import Data.Typeable ((:~:) (..))
import HasApply
import HasWord
import qualified Hoas.Bound as Bound
import Hoas.Type
import Id (Id)
import Term (Term)
import qualified Term
import Prelude hiding (curry, id, uncurry, (.), (<*>))

pointFree :: PointFree k a -> k '[] a
pointFree (PointFree x) = out x

newtype PointFree k a = PointFree (Pf k '[] a)

instance (forall env. HasApply (k env), Term k) => HasApply (PointFree k) where
  PointFree f <*> PointFree x = PointFree (f <*> x)

instance (forall env. HasWord (k env)) => HasWord (PointFree k) where
  u64 x = PointFree (to (u64 x))

instance Term k => Bound.Bound (PointFree k) where
  be n (PointFree x) t f = PointFree (Term.be x me)
    where
      v = Var t n
      PointFree body = f (PointFree (mkVar v))
      me = case removeVar body v of
        Nothing -> Term.const body
        Just y -> y

  lam n t f = PointFree (Term.curry me)
    where
      v = Var t n
      PointFree body = f (PointFree (mkVar v))
      me = case removeVar body v of
        Nothing -> Term.const body
        Just y -> y

  add = PointFree (to Term.add)

instance (forall env. HasApply (k env), Term k) => HasApply (Pf k env) where
  f <*> x = me
    where
      me =
        V
          { out = out f <*> out x,
            removeVar = \v -> case (removeVar f v, removeVar x v) of
              (Just f', Just x') -> Just (f' <*> x')
              (_, Just x') -> Just (Term.const f <*> x')
              (Just f', _) -> Just (f' <*> Term.const x)
              _ -> Nothing
          }

instance (forall env. HasWord (k env)) => HasWord (Pf k env) where
  u64 x = to (u64 x)

instance Term k => Term (Pf k) where
  add = to Term.add

  tip = me
    where
      me =
        V
          { out = Term.tip,
            removeVar = const Nothing
          }
  const f = me
    where
      me =
        V
          { out = Term.const (out f),
            removeVar = \v -> case removeVar f v of
              Just f' -> Just (Term.swap (Term.const f'))
              _ -> Nothing
          }

  be x f = me
    where
      me =
        V
          { out = out x `Term.be` out f,
            removeVar = \v -> case (removeVar x v, removeVar f v) of
              (Just x', Just f') -> Just (x' `Term.be` Term.swap f')
              (_, Just f') -> Just (Term.const x `Term.be` Term.swap f')
              (Just x', _) -> Just (x' `Term.be` Term.swap (Term.const f))
              _ -> Nothing
          }

  curry f = me
    where
      me =
        V
          { out = Term.curry (out f),
            removeVar = \v -> case removeVar f v of
              Nothing -> Nothing
              Just f' -> Just (Term.curry (Term.swap f'))
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

mkVar :: Term k => Var a -> Pf k '[] a
mkVar v@(Var _ n) = me
  where
    me =
      V
        { out = error ("free variable " ++ show n),
          removeVar = \maybeV -> case eqVar v maybeV of
            Nothing -> Nothing
            Just Refl -> Just (to Term.tip)
        }

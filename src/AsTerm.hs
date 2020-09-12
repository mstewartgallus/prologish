{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsTerm (PointFree, pointFree) where

import Data.Maybe
import Data.Typeable ((:~:) (..))
import qualified Hoas.Bound as Bound
import Hoas.Type
import Id (Id)
import Term (Term)
import qualified Term
import Prelude hiding (curry, id, uncurry, (.), (<*>))

pointFree :: PointFree k a -> k '[] a
pointFree (PointFree x) = out x

newtype PointFree k a = PointFree (Pf k '[] a)

instance Term k => Bound.Bound (PointFree k) where
  val (PointFree x) = PointFree (Term.val x)
  jump (PointFree k) (PointFree x) = PointFree (Term.jump k x)

  kont n t (PointFree x) f = PointFree (Term.kont x me)
    where
      v = Var t n
      PointFree body = f (PointFree (mkVar v))
      me = case removeVar body v of
        Nothing -> Term.const body
        Just y -> y

  u64 x = PointFree (lift0 (Term.u64 x))
  add (PointFree x) (PointFree y) = PointFree (Term.add x y)

instance Term k => Term (Pf k) where
  val = lift1 Term.val
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

  jump = lift2 Term.jump
  kont x y = me
    where
      me =
        V
          { out = Term.kont (out x) (out y),
            removeVar = \v -> error "todo"
          }

  u64 x = lift0 (Term.u64 x)
  add = lift2 Term.add

data Pf k env (b :: T) = V
  { out :: k env b,
    removeVar :: forall v. Var v -> Maybe (Pf k (v ': env) b)
  }

data Var a = Var (ST a) Id

eqVar :: Var a -> Var b -> Maybe (a :~: b)
eqVar (Var t m) (Var t' n)
  | m == n = eqT t t'
  | otherwise = Nothing

mkVar :: Term k => Var a -> Pf k '[] a
mkVar v@(Var _ n) = me
  where
    me =
      V
        { out = error ("free variable " ++ show n),
          removeVar = \maybeV -> case eqVar v maybeV of
            Nothing -> Nothing
            Just Refl -> Just (lift0 Term.tip)
        }

lift0 :: k a b -> Pf k a b
lift0 x = me
  where
    me =
      V
        { out = x,
          removeVar = const Nothing
        }

lift1 ::
  Term k =>
  (forall env. k env a -> k env c) ->
  Pf k env a ->
  Pf k env c
lift1 f x = me
  where
    me =
      V
        { out = f (out x),
          removeVar = \v -> case removeVar x v of
            Just x' -> Just $ lift1 f x'
            _ -> Nothing
        }

lift2 ::
  Term k =>
  (forall env. k env a -> k env b -> k env c) ->
  Pf k env a ->
  Pf k env b ->
  Pf k env c
lift2 f x y = me
  where
    me =
      V
        { out = f (out x) (out y),
          removeVar = \v -> case (removeVar x v, removeVar y v) of
            (Just x', Just y') -> Just $ lift2 f x' y'
            (_, Just y') -> Just $ lift2 f (Term.const x) y'
            (Just x', _) -> Just $ lift2 f x' (Term.const y)
            _ -> Nothing
        }

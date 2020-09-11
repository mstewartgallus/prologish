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
import Prelude hiding (id, (.), (<*>))

pointFree :: PointFree k a -> k a '[]
pointFree (PointFree x) = out x

newtype PointFree k a = PointFree (Pf k a '[])

instance Term k => Bound.Bound (PointFree k) where
  PointFree f `try` PointFree x = PointFree (f `Term.try` x)

  mal n t f = PointFree (Term.mal me)
    where
      v = Var t n
      PointFree body = f (PointFree (mkVar v))
      me = case removeVar body v of
        Nothing -> Term.const body
        Just y -> y

  isAbsurd = PointFree Term.absurd
  PointFree f ||| PointFree g = PointFree $ f Term.||| g

  PointFree x `isBoth` (PointFree f, PointFree g) = PointFree $ x `Term.isBoth` (f, g)

  PointFree x `isU64` n = PointFree (x `Term.isU64` n)
  add = PointFree Term.add
  isLeft (PointFree x) = PointFree $ Term.isLeft x

instance Term k => Term (Pf k) where
  add = to Term.add

  x `isU64` n = me
    where
      me =
        V
          { out = out x `Term.isU64` n,
            removeVar = \v -> case removeVar x v of
              Just x' -> Just $ x' `Term.isU64` n
              _ -> Nothing
          }

  x `isBoth` (f, g) = me
    where
      me =
        V
          { out = out x `Term.isBoth` (out f, out g),
            removeVar = \v -> case (removeVar x v, removeVar f v, removeVar g v) of
              (Nothing, Nothing, Just g') -> Just (Term.const x `Term.isBoth` (Term.const f, g'))
              (Nothing, Just f', Nothing) -> Just (Term.const x `Term.isBoth` (f', Term.const g))
              (Nothing, Just f', Just g') -> Just (Term.const x `Term.isBoth` (f', g'))
              (Just x', Nothing, Nothing) -> Just (x' `Term.isBoth` (Term.const f, Term.const g))
              (Just x', Nothing, Just g') -> Just (x' `Term.isBoth` (Term.const f, g'))
              (Just x', Just f', Just g') -> Just (x' `Term.isBoth` (f', g'))
              _ -> Nothing
          }

  f ||| g = me
    where
      me =
        V
          { out = out f Term.||| out g,
            removeVar = \v -> case (removeVar f v, removeVar g v) of
              (Just f', Just g') -> Just (f' Term.||| g')
              (_, Just g') -> Just (Term.const f Term.||| g')
              (Just f', _) -> Just (f' Term.||| Term.const g)
              _ -> Nothing
          }
  isLeft x = me
    where
      me =
        V
          { out = Term.isLeft $ out x,
            removeVar = \v -> case removeVar x v of
              Just x' -> Just $ Term.isLeft x'
              _ -> Nothing
          }

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

  f `try` x = me
    where
      me =
        V
          { out = out f `Term.try` out x,
            removeVar = \v -> case (removeVar f v, removeVar x v) of
              (Just f', Just x') -> Just (f' `Term.try` x')
              (_, Just x') -> Just (Term.const f `Term.try` x')
              (Just f', _) -> Just (f' `Term.try` Term.const x)
              _ -> Nothing
          }
  mal f = me
    where
      me =
        V
          { out = Term.mal (out f),
            removeVar = \v -> case removeVar f v of
              Nothing -> Nothing
              Just f' -> Just (Term.mal (Term.swap f'))
          }

data Pf k (b :: T) env = V
  { out :: k b env,
    removeVar :: forall v. Var v -> Maybe (Pf k b (v ': env))
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

mkVar :: Term k => Var a -> Pf k a '[]
mkVar v@(Var _ n) = me
  where
    me =
      V
        { out = error ("free variable " ++ show n),
          removeVar = \maybeV -> case eqVar v maybeV of
            Nothing -> Nothing
            Just Refl -> Just (to Term.tip)
        }

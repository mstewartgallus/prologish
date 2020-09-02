{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsPointFree (PointFree, pointFree) where

import Control.Category
import Data.Kind
import Data.Maybe
import Data.Typeable ((:~:) (..))
import Id (Id)
import Lambda
import Lambda.Exp
import Lambda.Product
import Lambda.Sum
import Lambda.Type
import qualified Term.Bound as Bound
import qualified Term.Type as Type
import Prelude hiding (curry, id, uncurry, (&&&), (.), (<*>))

pointFree :: PointFree k a b -> k a b
pointFree = out

data PointFree k a b = V
  { out :: k a b,
    removeVar :: forall v. Exp k => Var v -> Maybe (PointFree k (v * a) b)
  }

data Var a = Var (Type.ST a) Id

eqVar :: Var a -> Var b -> Maybe (a :~: b)
eqVar (Var t m) (Var t' n)
  | m == n = Type.eqT t t'
  | otherwise = Nothing

to :: k a b -> PointFree k a b
to x = me
  where
    me =
      V
        { out = x,
          removeVar = const Nothing
        }

instance Lambda k => Bound.Bound (PointFree k a) where
  f <*> x = me
    where
      me =
        V
          { out = out f <*> out x,
            removeVar = \v -> case (removeVar f v, removeVar x v) of
              (Just f', Just x') -> Just (f' <*> x')
              (_, Just x') -> Just ((f . second) <*> x')
              (Just f', _) -> Just (f' <*> (x . second))
              _ -> Nothing
          }

  lam id t f = curry me
    where
      v = Var t id
      body = f (mkVar v . unit)
      me = case removeVar body v of
        Nothing -> body . second
        Just y -> y

  u64 x = to (u64 x . unit)
  add = to (add . unit)

instance Category k => Category (PointFree k) where
  id = to id
  f . g = me
    where
      me =
        V
          { out = out f . out g,
            removeVar = \v -> case (removeVar f v, removeVar g v) of
              (Just f', Just g') -> Just (f' . (first &&& g'))
              (_, Just g') -> Just (f . g')
              (Just f', _) -> Just (f' . (first &&& (g . second)))
              _ -> Nothing
          }

mkVar :: Product k => Var a -> PointFree k Unit a
mkVar v@(Var _ n) = me
  where
    me =
      V
        { out = error ("free variable " ++ show n),
          removeVar = \maybeV -> case eqVar v maybeV of
            Nothing -> Nothing
            Just Refl -> Just (to first)
        }

instance Product k => Product (PointFree k) where
  unit = to unit
  first = to first
  second = to second
  f &&& g = me
    where
      me =
        V
          { out = out f &&& out g,
            removeVar = \v -> case (removeVar f v, removeVar g v) of
              (Just f', Just g') -> Just (f' &&& g')
              (_, Just g') -> Just ((f . second) &&& g')
              (Just f', _) -> Just (f' &&& (g . second))
              _ -> Nothing
          }

instance Sum k => Sum (PointFree k) where
  absurd = to absurd
  left = to left
  right = to right
  f ||| g = me
    where
      me =
        V
          { out = out f ||| out g,
            removeVar = \v -> case (removeVar f v, removeVar g v) of
              (Just f', Just g') -> Just (factor f' g')
              (_, Just g') -> Just (factor (f . second) g')
              (Just f', _) -> Just (factor f' (g . second))
              _ -> Nothing
          }

instance Exp k => Exp (PointFree k) where
  f <*> x = me
    where
      me =
        V
          { out = out f <*> out x,
            removeVar = \v -> case (removeVar f v, removeVar x v) of
              (Just f', Just x') -> Just (f' <*> x')
              (_, Just x') -> Just ((f . second) <*> x')
              (Just f', _) -> Just (f' <*> (x . second))
              _ -> Nothing
          }

  curry f = me
    where
      me =
        V
          { out = curry (out f),
            removeVar = \v -> case removeVar f v of
              Nothing -> Nothing
              Just f' -> Just (curry (f' . shuffle))
          }
      shuffle :: Product k => k (a * (b * c)) (b * (a * c))
      shuffle = (first . second) &&& (first &&& (second . second))
  uncurry f = me
    where
      me =
        V
          { out = uncurry (out f),
            removeVar = \v -> case removeVar f v of
              Nothing -> Nothing
              Just f' -> Just (uncurry f' . shuffle)
          }
      shuffle :: Product k => k (a * (b * c)) (b * (a * c))
      shuffle = (first . second) &&& (first &&& (second . second))

factor :: (Sum k, Exp k) => k (x * a) c -> k (x * b) c -> k (x * (a + b)) c
factor f g = uncurry (curry f ||| curry g)

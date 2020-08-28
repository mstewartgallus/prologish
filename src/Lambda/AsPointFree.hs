{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Lambda.AsPointFree (PointFree, pointFree) where

import Control.Category
import Data.Kind
import Data.Maybe
import Data.Typeable ((:~:) (..))
import Lambda.Exp
import Id (Id)
import Lambda.Labels
import Lambda
import Lambda.Product
import Lambda.Sum
import Lambda.Type
import Lambda.Vars
import Prelude hiding ((.), (<*>), id, (&&&), curry, uncurry)

pointFree :: PointFree k a b -> k a b
pointFree = out

data PointFree k a b = V
  { out :: k a b,
    removeVar :: forall v. (Exp k, Product k) => Var v -> Maybe (PointFree k (v * a) b),
    removeLabel :: forall v. (Exp k, Sum k) => Label v -> Maybe (PointFree k a (v + b))
  }

data Label a = Label (ST a) Id

eqLabel :: Label a -> Label b -> Maybe (a :~: b)
eqLabel (Label t m) (Label t' n)
  | m == n = eqT t t'
  | otherwise = Nothing

data Var a = Var (ST a) Id

eqVar :: Var a -> Var b -> Maybe (a :~: b)
eqVar (Var t m) (Var t' n)
  | m == n = eqT t t'
  | otherwise = Nothing

to :: k a b -> PointFree k a b
to x = me
  where
    me =
      V
        { out = x,
          removeVar = const Nothing,
          removeLabel = const Nothing
        }

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
              _ -> Nothing,
            removeLabel = \v -> case (removeLabel f v, removeLabel g v) of
              (Just f', Just g') -> Just ((left ||| f') . g')
              (_, Just g') -> Just ((left ||| (right . f)) . g')
              (Just f', _) -> Just (f' . g)
              _ -> Nothing
          }

instance (Exp k, Sum k) => Labels (PointFree k) where
  bindImplicitLabel n t f x = me
    where
      v = Label t n
      body = f (mkLabel v)
      me = case removeLabel body v of
        Nothing -> absurd . body
        Just y -> (x ||| absurd) . y

instance Exp k => Vars (PointFree k) where
  bindImplicitEnv n t f x = me where
      v = Var t n
      body = f (mkVar v)
      me = case removeVar body v of
        Nothing -> body . unit
        Just y -> y . (x &&& unit)

mkVar :: Product k => Var a -> PointFree k Unit a
mkVar v@(Var _ n) = me
  where
    me =
      V
        { out = error ("free variable " ++ show n),
          removeVar = \maybeV -> case eqVar v maybeV of
            Nothing -> Nothing
            Just Refl -> Just (to first),
          removeLabel = const Nothing
        }

mkLabel :: Sum k => Label a -> PointFree k a Void
mkLabel v@(Label _ n) = me
  where
    me =
      V
        { out = error ("free label " ++ show n),
          removeLabel = \maybeV -> case eqLabel v maybeV of
            Nothing -> Nothing
            Just Refl -> Just (to left),
          removeVar = const Nothing
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
              _ -> Nothing,
            removeLabel = \v -> case (removeLabel f v, removeLabel g v) of
              (Just f', Just g') -> Just (distribute f' g')
              (_, Just g') -> Just (distribute (right . f) g')
              (Just f', _) -> Just (distribute f' (right . g))
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
              _ -> Nothing,
            removeLabel = \v -> case (removeLabel f v, removeLabel g v) of
              (Just f', Just g') -> Just (f' ||| g')
              (_, Just g') -> Just ((right . f) ||| g')
              (Just f', _) -> Just (f' ||| (right . g))
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

instance Lambda k => Lambda (PointFree k) where
  u64 x = to (u64 x)
  add = to add

factor :: (Sum k, Exp k) => k (x * a) c -> k (x * b) c -> k (x * (a + b)) c
factor f g = uncurry (curry f ||| curry g)

distribute :: (Sum k, Exp k) => k c (x + a) -> k c (x + b) -> k c (x + (a * b))
distribute f g = undefined

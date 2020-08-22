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
import Prelude hiding ((.), (<*>), id)

pointFree :: PointFree k a b -> k a b
pointFree = out

data PointFree k a b = V
  { out :: k a b,
    removeVar :: forall v. (Exp k, Product k) => Var v -> Maybe (PointFree k (a * v) b),
    removeLabel :: forall v. (Exp k, Sum k) => Label v -> Maybe (PointFree k a (b + v))
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
              (Just f', Just g') -> Just (f' . (g' # second))
              (_, Just g') -> Just (f . g')
              (Just f', _) -> Just (f' . ((g . first) # second))
              _ -> Nothing,
            removeLabel = \v -> case (removeLabel f v, removeLabel g v) of
              (Just f', Just g') -> Just ((f' ! right) . g')
              (_, Just g') -> Just (((left . f) ! right) . g')
              (Just f', _) -> Just (f' . g)
              _ -> Nothing
          }

instance (Exp k, Sum k) => Labels (PointFree k) where
  bindMapLabel n t f = me
    where
      v = Label t n
      body = f (mkLabel v)
      me = case removeLabel body v of
        Nothing -> absurd . body
        Just x -> (absurd ! id) . x

instance Exp k => Vars (PointFree k) where
  bindMapVar n t f = me
    where
      v = Var t n
      body = f (mkVar v)
      me = case removeVar body v of
        Nothing -> body . unit
        Just x -> x . (unit # id)

mkVar :: Product k => Var a -> PointFree k Unit a
mkVar v@(Var _ n) = me
  where
    me =
      V
        { out = error ("free variable " ++ show n),
          removeVar = \maybeV -> case eqVar v maybeV of
            Nothing -> Nothing
            Just Refl -> Just (to second),
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
            Just Refl -> Just (to right),
          removeVar = const Nothing
        }

instance Product k => Product (PointFree k) where
  unit = to unit
  first = to first
  second = to second
  f # g = me
    where
      me =
        V
          { out = out f # out g,
            removeVar = \v -> case (removeVar f v, removeVar g v) of
              (Just f', Just g') -> Just (f' # g')
              (_, Just g') -> Just ((f . first) # g')
              (Just f', _) -> Just (f' # (g . first))
              _ -> Nothing,
            removeLabel = \v -> case (removeLabel f v, removeLabel g v) of
              (Just f', Just g') -> Just (distribute f' g')
              (_, Just g') -> Just (distribute (left . f) g')
              (Just f', _) -> Just (distribute f' (left . g))
              _ -> Nothing
          }

instance Sum k => Sum (PointFree k) where
  absurd = to absurd
  left = to left
  right = to right
  f ! g = me
    where
      me =
        V
          { out = out f ! out g,
            removeVar = \v -> case (removeVar f v, removeVar g v) of
              (Just f', Just g') -> Just (factor f' g')
              (_, Just g') -> Just (factor (f . first) g')
              (Just f', _) -> Just (factor f' (g . first))
              _ -> Nothing,
            removeLabel = \v -> case (removeLabel f v, removeLabel g v) of
              (Just f', Just g') -> Just (f' ! g')
              (_, Just g') -> Just ((left . f) ! g')
              (Just f', _) -> Just (f' ! (left . g))
              _ -> Nothing
          }

instance Exp k => Exp (PointFree k) where
  eval f = me
    where
      me =
        V
          { out = eval (out f),
            removeVar = \v -> case removeVar f v of
              Nothing -> Nothing
              Just f' -> Just (eval f' . shuffle)
          }
      shuffle :: Product k => k ((a * c) * b) ((a * b) * c)
      shuffle = ((first . first) # second) # (second . first)

  lambda f = me
    where
      me =
        V
          { out = lambda (out f),
            removeVar = \v -> case removeVar f v of
              Nothing -> Nothing
              Just f' -> Just (lambda (f' . shuffle))
          }
      shuffle :: Product k => k ((a * c) * b) ((a * b) * c)
      shuffle = ((first . first) # second) # (second . first)

instance Lambda k => Lambda (PointFree k) where
  u64 x = to (u64 x)
  add = to add

factor :: (Sum k, Exp k) => k (a * x) c -> k (b * x) c -> k ((a + b) * x) c
factor f g = eval (lambda f ! lambda g)

distribute :: (Sum k, Exp k) => k c (a + x) -> k c (b + x) -> k c ((a * b) + x)
distribute f g = eval id . ((process . f) # g)

process :: (Sum k, Exp k) => k (a + x) ((b + x) ~> ((a * b) + x))
process = lambda (eval id . ((bar . second) # first)) ! lambda (right . first)

bar :: (Sum k, Exp k) => k (b + x) (a ~> ((a * b) + x))
bar = lambda (left . (second # first)) ! lambda (right . first)

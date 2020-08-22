{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsPointFree (PointFree, pointFree) where

import Control.Category
import Data.Kind
import Data.Maybe
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

pointFree :: PointFree k a b -> k a b
pointFree = out

data PointFree k a b = V
  { out :: k a b,
    hasVar :: forall v. Var v -> Bool,
    removeVar :: forall v. (Exp k, Product k) => Var v -> PointFree k (a * v) b,
    removeLabel :: forall v. (Exp k, Sum k) => Label v -> PointFree k a (b + v)
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
          hasVar = const False,
          removeVar = const (me . first),
          removeLabel = const (left . me)
        }

instance Category k => Category (PointFree k) where
  id = to id
  f . g = me
    where
      me =
        V
          { out = out f . out g,
            hasVar = \v -> hasVar f v || hasVar g v,
            removeVar = \v -> case (hasVar f v, hasVar g v) of
              (False, False) -> f . g . first
              (False, _) -> f . removeVar g v
              (_, False) -> removeVar f v . ((g . first) # second)
              _ -> removeVar f v . (removeVar g v # second),
            removeLabel = \v -> (removeLabel f v ! right) . removeLabel g v
          }

instance (Exp k, Sum k) => Labels (PointFree k) where
  bindMapLabel n t f = me
    where
      v = Label t n
      me = (absurd ! id) . removeLabel (f (mkLabel v)) v

instance Exp k => Vars (PointFree k) where
  bindMapVar n t f = me
    where
      v = Var t n
      me = removeVar (f (mkVar v)) v . (unit # id)

mkVar :: Product k => Var a -> PointFree k Unit a
mkVar v@(Var _ n) = me
  where
    me =
      V
        { out = error ("free variable " ++ show n),
          hasVar = \v' -> isJust (eqVar v v'),
          removeVar = \maybeV -> case eqVar v maybeV of
            Nothing -> me . first
            Just Refl -> to second,
          removeLabel = const (left . me)
        }

mkLabel :: Sum k => Label a -> PointFree k a Void
mkLabel v@(Label _ n) = me
  where
    me =
      V
        { out = error ("free label " ++ show n),
          hasVar = const False,
          removeLabel = \maybeV -> case eqLabel v maybeV of
            Nothing -> left . me
            Just Refl -> to right,
          removeVar = const (me . first)
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
            hasVar = \v -> hasVar f v || hasVar g v,
            removeVar = \v -> case (hasVar f v, hasVar g v) of
              (False, False) -> (f # g) . first
              (False, _) -> ((f . first) # removeVar g v)
              (_, False) -> removeVar f v # (g . first)
              _ -> removeVar f v # removeVar g v,
            removeLabel = \v -> distribute (removeLabel f v) (removeLabel g v)
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
            hasVar = \v -> hasVar f v || hasVar g v,
            removeVar = \v -> factor (removeVar f v) (removeVar g v),
            removeLabel = \v -> removeLabel f v ! removeLabel g v
          }

instance Exp k => Exp (PointFree k) where
  eval = to eval
  lambda f = me
    where
      me =
        V
          { out = lambda (out f),
            hasVar = hasVar f,
            removeVar = \v -> lambda (removeVar f v . shuffle),
            removeLabel = undefined
          }
      shuffle :: Product k => k ((a * c) * b) ((a * b) * c)
      shuffle = ((first . first) # second) # (second . first)

instance Lambda k => Lambda (PointFree k) where
  u64 x = to (u64 x)
  add = to add

factor :: (Sum k, Exp k) => k (a * x) c -> k (b * x) c -> k ((a + b) * x) c
factor f g = eval . (((lambda f ! lambda g) . first) # second)

distribute :: (Sum k, Exp k) => k c (a + x) -> k c (b + x) -> k c ((a * b) + x)
distribute f g = eval . ((process . f) # g)

process :: (Sum k, Exp k) => k (a + x) ((b + x) ~> ((a * b) + x))
process = lambda (eval . ((bar . second) # first)) ! lambda (right . first)

bar :: (Sum k, Exp k) => k (b + x) (a ~> ((a * b) + x))
bar = lambda (left . (second # first)) ! lambda (right . first)

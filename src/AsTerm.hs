{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsTerm (PointFree, pointFree) where

import Control.Category
import Data.Kind
import Data.Maybe
import Data.Typeable ((:~:) (..))
import Global
import HasCoexp
import HasProduct
import HasSum
import qualified Hoas.Bound as Bound
import Id (Id)
import Mal
import Type
import Prelude hiding (curry, id, uncurry, (.), (<*>))

pointFree :: PointFree k a b -> k a b
pointFree (E x) = out x

data PointFree (k :: T -> T -> Type) a b = E (Pf k (a) (b))

instance Mal k => Category (PointFree k) where
  E f . E g = E (f . g)
  id = E id

instance Mal k => Bound.Cokappa (PointFree k) where
  label n t f = E (try me)
    where
      k = Label t n
      E body = f (E (mkLabel k))

      me = case removeLabel body k of
        Nothing -> mal (right . body)
        Just y -> y
  lift (E x) = E ((absurd . x) ||| id)

instance Mal k => Bound.Cozeta (PointFree k) where
  mal n t f = E me
    where
      k = Label t n
      E body = f (E (mkLabel k))

      me = case removeLabel body k of
        Nothing -> mal (right . body)
        Just y -> y
  pass (E x) = E (id <*> (absurd . x))

instance Mal k => Bound.Bound (PointFree k) where
  u64 x = E (u64 x . unit)
  global g = E (global g)

instance Mal k => Category (Pf k) where
  id = lift0 id
  f . g = me
    where
      me =
        V
          { out = out f . out g,
            removeLabel = \v -> case (removeLabel f v, removeLabel g v) of
              (Just f', Just g') -> Just $ mal ((left ||| try f') . try g')
              (_, Just g') -> Just $ f . g'
              (Just f', _) -> Just $ mal (try f' . g)
              _ -> Nothing
          }

instance Mal k => HasSum (Pf k) where
  absurd = lift0 absurd
  left = lift0 left
  right = lift0 right
  x ||| y = me
    where
      me =
        V
          { out = out x ||| out y,
            removeLabel = \v -> case (removeLabel x v, removeLabel y v) of
              (Just x', Just y') -> Just $ mal (try x' ||| try y')
              (_, Just y') -> Just $ mal ((right . x) ||| try y')
              (Just x', _) -> Just $ mal (try x' ||| (right . y))
              _ -> Nothing
          }

instance Mal k => HasProduct (Pf k) where
  unit = lift0 unit
  first = lift0 first
  second = lift0 second
  (&&&) = lift2 (&&&)

instance Mal k => HasCoexp (Pf k) where
  mal f = me
    where
      me =
        V
          { out = mal $ out f,
            removeLabel = \v -> case removeLabel f v of
              Just f' -> Just $ mal $ mal (shuffleSum (try f'))
              _ -> Nothing
          }
  try f = me
    where
      me =
        V
          { out = try $ out f,
            removeLabel = \v -> case removeLabel f v of
              Just f' -> Just $ mal (shuffleSum (try (try f')))
              _ -> Nothing
          }

shuffleSum :: HasSum k => k b (a + (v + c)) -> k b (v + (a + c))
shuffleSum x = ((right . left) ||| (left ||| (right . right))) . x

instance Mal k => Mal (Pf k) where
  global g = lift0 $ global g
  u64 x = lift0 $ u64 x

data Pf k (a :: T) (b :: T) = V
  { out :: k a b,
    removeLabel :: forall v. Label v -> Maybe (Pf k (v -< a) b)
  }

data Label a = Label (ST a) Id

eqLabel :: Label a -> Label b -> Maybe (a :~: b)
eqLabel (Label t m) (Label t' n)
  | m == n = eqT t t'
  | otherwise = Nothing

mkLabel :: HasCoexp k => Label a -> Pf k (a) Void
mkLabel v@(Label _ n) = me
  where
    me =
      V
        { out = error ("free label " ++ show n),
          removeLabel = \maybeV -> case eqLabel v maybeV of
            Nothing -> Nothing
            Just Refl -> Just (lift0 (mal left))
        }

lift0 :: k a b -> Pf k a b
lift0 x = me
  where
    me =
      V
        { out = x,
          removeLabel = const Nothing
        }

lift2 ::
  Mal k =>
  (forall env. k env a -> k env b -> k env c) ->
  Pf k env a ->
  Pf k env b ->
  Pf k env c
lift2 f x y = me
  where
    me =
      V
        { out = f (out x) (out y),
          removeLabel = \v -> case (removeLabel x v, removeLabel y v) of
            (Just x', Just y') -> error "foo"
            (_, Just y') -> error "foo"
            (Just x', _) -> error "foo"
            _ -> Nothing
        }

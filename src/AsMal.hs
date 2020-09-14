{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsMal (PointFree, asMal, foo, AsObject) where

import Control.Category
import Data.Maybe
import Data.Typeable ((:~:) (..))
import qualified Hoas.Bound as Bound
import qualified Hoas.Type as Type
import Id (Id)
import Mal
import Mal.HasCoexp
import Mal.HasProduct
import Mal.HasSum
import Mal.Type
import Prelude hiding (curry, id, uncurry, (.), (<*>))

type family AsObject a = r | r -> a where
  AsObject (a Type.+ b) = AsObject a + AsObject b
  AsObject (a Type.* b) = AsObject a * AsObject b
  AsObject (a Type.-< b) = AsObject a -< AsObject b
  AsObject Type.Void = Void
  AsObject Type.Unit = Unit
  AsObject Type.B = B
  AsObject Type.U64 = U64

asMal :: PointFree k a -> k Unit (AsObject a)
asMal (PointFree x) = out x

newtype PointFree k a = PointFree (Pf k Unit (AsObject a))

instance Mal k => Bound.Bound (PointFree k) where
  val (PointFree x) = PointFree (val x)
  jump (PointFree k) (PointFree x) = PointFree (jump k (x . unit))

  kont n t (PointFree x) f = PointFree (kont x me)
    where
      v = Var t n
      PointFree body = f (PointFree (mkVar v))
      me = case removeVar body v of
        Nothing -> body . unit
        Just y -> y . (id &&& unit)

  unit = PointFree unit
  first (PointFree x) = PointFree (first . x)
  second (PointFree x) = PointFree (second . x)

  absurd (PointFree x) = PointFree (absurd . x)
  left (PointFree x) = PointFree (left . x)
  right (PointFree x) = PointFree (right . x)

  u64 x = PointFree (u64 x)
  add (PointFree x) (PointFree y) = PointFree (add x y)

instance Mal k => Category (Pf k) where
  id = lift0 id
  f . g = me
    where
      me =
        V
          { out = out f . out g,
            removeVar = \v -> case (removeVar f v, removeVar g v) of
              (Just f', Just g') -> Just $ f' . (first &&& g')
              (_, Just g') -> Just $ f . g'
              (Just f', _) -> Just $ f' . (first &&& (g . second))
              _ -> Nothing
          }

instance Mal k => HasSum (Pf k) where
  absurd = lift0 absurd
  f ||| g = me
    where
      me =
        V
          { out = out f ||| out g,
            removeVar = \v -> case (removeVar f v, removeVar g v) of
              (Just f', Just g') -> Just $ (f' ||| g') . factorIn
              _ -> error "todo"
          }
  left = lift0 left
  right = lift0 right

instance Mal k => HasProduct (Pf k) where
  unit = lift0 unit
  (&&&) = lift2 (&&&)
  first = lift0 first
  second = lift0 second

instance Mal k => HasCoexp (Pf k) where
  -- fixme... figure out which case are linearity error and which are
  -- not
  kont x k = me
    where
      me =
        V
          { out = kont (out x) (out k),
            removeVar = \v -> case (removeVar x v, removeVar k v) of
              (Just _, Just _) -> error "todo"
              (_, Just _) -> error "todo"
              (Just _, _) -> error "todo"
              _ -> Nothing
          }
  val = lift1 val
  jump k x = me
    where
      me =
        V
          { out = jump (out k) (out x),
            removeVar = \v -> case (removeVar k v, removeVar x v) of
              (Just _, Just _) -> error "todo"
              (_, Just _) -> error "todo"
              (Just k', _) -> Just $ jump k' x
              _ -> Nothing
          }

foo f = factorOut . mal ((left ||| (right . factorIn)) . f)

bar :: Mal k => k (v * b) (a + env) -> k (v * (a -< b)) env
bar f = undefined

instance Mal k => Mal (Pf k) where
  u64 x = lift0 (u64 x)
  add = lift2 add

data Pf k env (b :: T) = V
  { out :: k env b,
    removeVar :: forall v. Var v -> Maybe (Pf k (AsObject v * env) b)
  }

data Var a = Var (Type.ST a) Id

eqVar :: Var a -> Var b -> Maybe (a :~: b)
eqVar (Var t m) (Var t' n)
  | m == n = Type.eqT t t'
  | otherwise = Nothing

mkVar :: Mal k => Var a -> Pf k Unit (AsObject a)
mkVar v@(Var _ n) = me
  where
    me =
      V
        { out = error ("free variable " ++ show n),
          removeVar = \maybeV -> case eqVar v maybeV of
            Nothing -> Nothing
            Just Refl -> Just (lift0 first)
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
  Mal k =>
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
          removeVar = \v -> case (removeVar x v, removeVar y v) of
            (Just x', Just y') -> Just $ lift2 f x' y'
            (_, Just y') -> Just $ lift2 f (x . second) y'
            (Just x', _) -> Just $ lift2 f x' (y . second)
            _ -> Nothing
        }

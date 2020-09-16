{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsTerm (PointFree, pointFree, AsObject) where

import Control.Category
import Data.Kind
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
  AsObject Type.Unit = Unit
  AsObject Type.Void = Void
  AsObject Type.U64 = U64

pointFree :: PointFree k a b -> k (AsObject a) (AsObject b)
pointFree (E x) = out x

data PointFree (k :: T -> T -> Type) a b = E (Pf k (AsObject a) (AsObject b))

instance Mal k => Bound.Bound (PointFree k) where
  E f `try` E x = E (f <*> x)
  E f ||| E x = E (f ||| x)

  E f `jump` E g = E (f . g)

  mal n t f = E (mal me)
    where
      k = Label t n
      E body = f (E (mkLabel k))

      me = case removeLabel body k of
        Nothing -> right . body
        Just y -> y

  thunk n t f = E me
    where
      v = Label t n
      E body = f (E (mkLabel v))
      me = case removeLabel body v of
        Nothing -> absurd . body
        Just y -> (id ||| absurd) . y
  letBe n t f = E me
    where
      v = Var t n
      E body = f (E (mkVar v))
      me = case removeVar body v of
        Nothing -> body . unit
        Just y -> y . (id &&& unit)

  unit = E unit
  empty = E absurd

  u64 x = E (u64 x . unit)

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
              _ -> Nothing,
            removeLabel = \v -> case (removeLabel f v, removeLabel g v) of
              (Just f', Just g') -> Just $ (left ||| f') . g'
              (_, Just g') -> Just $ (left ||| (right . f)) . g'
              (Just f', _) -> Just $ f' . g
              _ -> Nothing
          }

instance Mal k => HasSum (Pf k) where
  absurd = lift0 absurd
  left = lift0 left
  right = lift0 right
  (|||) = colift2 (|||)

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
            removeVar = \v -> case removeVar f v of
              Just f' -> Just $ swp (mal f')
              _ -> Nothing,
            removeLabel = \v -> case removeLabel f v of
              Just f' -> Just $ mal (shuffleSum f')
              _ -> Nothing
          }
  try f = me
    where
      me =
        V
          { out = try $ out f,
            removeVar = \v -> case removeVar f v of
              Just f' -> error "todo"
              _ -> Nothing,
            removeLabel = \v -> case removeLabel f v of
              Just f' -> Just $ shuffleSum (try f')
              _ -> Nothing
          }

-- | Impossible for the same reasons
-- k c (a -> (v + b)) -> k c (v + (a -> b))
-- is impossible ...
swp :: Mal k => k (a -< (v * b)) c -> k (v * (a -< b)) c
swp _ = error "impossible"

shuffleSum :: HasSum k => k b (a + (v + c)) -> k b (v + (a + c))
shuffleSum x = ((right . left) ||| (left ||| (right . right))) . x

instance Mal k => Mal (Pf k) where
  u64 x = lift0 $ u64 x

data Pf k env (b :: T) = V
  { out :: k env b,
    removeVar :: forall v. Var v -> Maybe (Pf k (AsObject v * env) b),
    removeLabel :: forall v. Label v -> Maybe (Pf k env (AsObject v + b))
  }

data Var a = Var (Type.ST a) Id

data Label a = Label (Type.ST a) Id

eqVar :: Var a -> Var b -> Maybe (a :~: b)
eqVar (Var t m) (Var t' n)
  | m == n = Type.eqT t t'
  | otherwise = Nothing

eqLabel :: Label a -> Label b -> Maybe (a :~: b)
eqLabel (Label t m) (Label t' n)
  | m == n = Type.eqT t t'
  | otherwise = Nothing

mkVar :: HasProduct k => Var a -> Pf k x (AsObject a)
mkVar v@(Var _ n) = me
  where
    me =
      V
        { out = error ("free variable " ++ show n),
          removeVar = \maybeV -> case eqVar v maybeV of
            Nothing -> Nothing
            Just Refl -> Just (lift0 first),
          removeLabel = const Nothing
        }

mkLabel :: HasSum k => Label a -> Pf k (AsObject a) x
mkLabel v@(Label _ n) = me
  where
    me =
      V
        { out = error ("free label " ++ show n),
          removeLabel = \maybeV -> case eqLabel v maybeV of
            Nothing -> Nothing
            Just Refl -> Just (lift0 left),
          removeVar = const Nothing
        }

lift0 :: k a b -> Pf k a b
lift0 x = me
  where
    me =
      V
        { out = x,
          removeLabel = const Nothing,
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
          removeLabel = \v -> case (removeLabel x v, removeLabel y v) of
            (Just x', Just y') -> error "foo"
            (_, Just y') -> error "foo"
            (Just x', _) -> error "foo"
            _ -> Nothing,
          removeVar = \v -> case (removeVar x v, removeVar y v) of
            (Just x', Just y') -> Just $ lift2 f x' y'
            (_, Just y') -> Just $ lift2 f (x . second) y'
            (Just x', _) -> Just $ lift2 f x' (y . second)
            _ -> Nothing
        }

colift2 ::
  Mal k =>
  (forall env. k a env -> k b env -> k c env) ->
  Pf k a env ->
  Pf k b env ->
  Pf k c env
colift2 f x y = me
  where
    me =
      V
        { out = f (out x) (out y),
          removeVar = \v -> case (removeVar x v, removeVar y v) of
            (Just x', Just y') -> error "todo"
            (Just x', _) -> error "todo"
            (_, Just y') -> error "todo"
            _ -> Nothing,
          removeLabel = \v -> case (removeLabel x v, removeLabel y v) of
            (Just x', Just y') -> Just $ colift2 f x' y'
            (_, Just y') -> error "foo"
            (Just x', _) -> error "foo"
            _ -> Nothing
        }

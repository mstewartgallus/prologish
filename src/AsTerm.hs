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

instance Mal k => Category (PointFree k) where
  E f . E g = E (f . g)
  id = E id

instance Mal k => Bound.Bound (PointFree k) where
  E f `try` E x = E (f <*> x)
  mal (E f) = E (mal f)

  letLabel n t f = E me
    where
      k = Label t n
      E body = f (E (mkLabel k))

      me = case removeLabel body k of
        Nothing -> right . body
        Just y -> y

  unit = E unit
  E f &&& E x = E (f &&& x)
  first (E x) = E (first . x)
  second (E x) = E (second . x)

  absurd = E absurd
  E f ||| E x = E (f ||| x)
  left (E x) = E (x . left)
  right (E x) = E (x . right)

  u64 x = E (u64 x . unit)
  add = E (lift add)

lift :: Mal k => k a b -> k (b -< a) Void
lift x = mal (left . x)

instance Mal k => Category (Pf k) where
  id = lift0 id
  f . g = me
    where
      me =
        V
          { out = out f . out g,
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
            removeLabel = \v -> case removeLabel f v of
              Just f' -> Just $ mal (shuffleSum f')
              _ -> Nothing
          }
  try f = me
    where
      me =
        V
          { out = try $ out f,
            removeLabel = \v -> case removeLabel f v of
              Just f' -> Just $ shuffleSum (try f')
              _ -> Nothing
          }

shuffleSum :: HasSum k => k b (a + (v + c)) -> k b (v + (a + c))
shuffleSum x = ((right . left) ||| (left ||| (right . right))) . x

instance Mal k => Mal (Pf k) where
  add = lift0 add
  u64 x = lift0 $ u64 x

data Pf k (a :: T) (b :: T) = V
  { out :: k a b,
    removeLabel :: forall v. Label v -> Maybe (Pf k a (AsObject v + b))
  }

data Label a = Label (Type.ST a) Id

eqLabel :: Label a -> Label b -> Maybe (a :~: b)
eqLabel (Label t m) (Label t' n)
  | m == n = Type.eqT t t'
  | otherwise = Nothing

mkLabel :: HasSum k => Label a -> Pf k (AsObject a) Void
mkLabel v@(Label _ n) = me
  where
    me =
      V
        { out = error ("free label " ++ show n),
          removeLabel = \maybeV -> case eqLabel v maybeV of
            Nothing -> Nothing
            Just Refl -> Just (lift0 left)
        }

lift0 :: k a b -> Pf k a b
lift0 x = me
  where
    me =
      V
        { out = x,
          removeLabel = const Nothing
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
        { out = f (out x)
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
          removeLabel = \v -> case (removeLabel x v, removeLabel y v) of
            (Just x', Just y') -> Just $ colift2 f x' y'
            (_, Just y') -> Just $ colift2 f (right . x) y'
            (Just x', _) -> Just $ colift2 f x' (right . y)
            _ -> Nothing
        }

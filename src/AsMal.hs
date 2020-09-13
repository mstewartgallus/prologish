{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module AsMal (PointFree, AsObject, asMal) where

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
  AsObject (a Type.|- b) = AsObject a |- AsObject b
  AsObject (a Type.* b) = AsObject a * AsObject b
  AsObject Type.Void = Void
  AsObject Type.Unit = Unit
  AsObject Type.B = B
  AsObject Type.U64 = U64

asMal :: PointFree k a -> k Unit (AsObject a)
asMal (PointFree x) = out x

newtype PointFree k a = PointFree (Pf k Unit (AsObject a))

instance Mal k => Bound.Bound (PointFree k) where
  val (PointFree x) = PointFree (mal right . x)
  jump (PointFree k) (PointFree x) = PointFree (mal (left . x . unit) . k)

  kont n t (PointFree x) f = PointFree (((absurd . me) ||| id) . try id . x)
    where
      v = Var t n
      PointFree body = f (PointFree (mkVar v))
      me = case removeVar body v of
        Nothing -> body . unit
        Just y -> y . (id &&& unit)

  -- unit = PointFree Term.unit
  left (PointFree x) = PointFree (left . x)
  right (PointFree x) = PointFree (right . x)

  u64 x = PointFree (lift0 (u64 x))
  add (PointFree x) (PointFree y) = PointFree (add x y)

instance Mal k => Category (Pf k) where
  id = lift0 id
  f . g = me
    where
      me =
        V
          { out = out f . out g,
            removeVar = \v -> case (removeVar f v, removeVar g v) of
              (Just f', Just g') -> Just (f' . (first &&& g'))
              (Just f', _) -> Just (f' . (first &&& (g . second)))
              (_, Just g') -> Just (f . g')
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
              -- (Just f', Just g') -> Just (factor (f' ||| g'))
              -- (Just f', _) -> Just (f' ||| g)
              -- (_, Just g') -> Just (f ||| g')
              _ -> error "todo"
          }
  left = lift0 left
  right = lift0 right

factor :: Mal k => k ((v * a) + (v * b)) r -> k (v * (a + b)) r
factor f = undefined

-- -- g :: Mal k => k (a0 -< ((v * a) + (v * b))) r -> k (a0 -< (v * (a + b))) r
-- g f = undefined

instance Mal k => HasProduct (Pf k) where
  unit = lift0 unit

instance Mal k => HasCoexp (Pf k) where
  mal x = me
    where
      me =
        V
          { out = mal (out x),
            removeVar = \v -> case removeVar x v of
              Just x' -> error "todo"
              _ -> Nothing
          }

-- swap :: (HasProduct k, HasCoexp k) => k (v -< (a * b)) r -> k (a * (v -< b)) r
-- swap f = let
--   () = try f
--   in undefined

instance Mal k => Mal (Pf k) where
  --   tip = me
  --     where
  --       me =
  --         V
  --           { out = Term.tip,
  --             removeVar = const Nothing
  --           }

  --   const f = me
  --     where
  --       me =
  --         V
  --           { out = Term.const (out f),
  --             removeVar = \v -> case removeVar f v of
  --               Just f' -> Just (Term.swap (Term.const f'))
  --               _ -> Nothing
  --           }
  --   swap x = me
  --     where
  --       me =
  --         V
  --           { out = Term.swap (out x),
  --             removeVar = \v -> case removeVar x v of
  --               -- Just x' -> Just (Term.const x')
  --               _ -> error "todo"
  --           }

  --   try x y z = me
  --     where
  --       me =
  --         V
  --           { out = Term.try (out x) (out y) (out z),
  --             removeVar = \v -> case (removeVar x v, removeVar y v, removeVar z v) of
  --               -- (Just x', Just y', Just z') -> Just $ Term.try x' y' z'
  --               -- (_, Just y') -> Just $ Term.mal (Term.swap (Term.const x)) y'
  --               -- (Just x', _) -> Just $ Term.mal (Term.swap x') (Term.const y)
  --               _ -> error "todo"
  --           }

  --   unit = lift0 Term.unit

  --   absurd = lift1 Term.absurd
  --   left = lift1 Term.left
  --   right = lift1 Term.right

  u64 x = lift0 (u64 x)
  add = lift2 add

data Pf k env (b :: T) = V
  { out :: k env b,
    removeVar :: forall v. Var v -> Maybe (Pf k ((AsObject v) * env) b)
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
